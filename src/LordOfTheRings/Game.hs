{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally
import LordOfTheRings.Attachment
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy
import LordOfTheRings.Entities
import LordOfTheRings.Frontend
import LordOfTheRings.Effect
import LordOfTheRings.Helpers.Attachment
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Modifiers
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Token
import LordOfTheRings.Helpers.Window
import LordOfTheRings.Ability.Types
import LordOfTheRings.Source
import LordOfTheRings.Game.Base as X
import LordOfTheRings.GameT
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Location
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Player.Runner as X
import LordOfTheRings.Prelude
import LordOfTheRings.Quest
import LordOfTheRings.Question
import LordOfTheRings.Scenario
import LordOfTheRings.Target
import LordOfTheRings.Keyword
import LordOfTheRings.Treachery
import LordOfTheRings.Window

newGame :: Player -> [Hero] -> Game
newGame player heroes =
  Game
    { gamePlayers = Map.singleton player.id player
    , gamePlayerOrder = [player.id]
    , gameActivePlayer = player.id
    , gameFirstPlayer = player.id
    , gameEntities = mempty {entitiesHeroes = Map.fromList [(h.id, h) | h <- heroes]}
    , gameScenario = passageThroughMirkwood
    , gamePhase = ResourcePhase
    , gameRound = 0
    , gameQuestion = mempty
    , gameQuestionVersion = 0
    , gameActiveCosts = mempty
    , gameModifiers = mempty
    , gameEffects = []
    , gameOutcome = Nothing
    }

answerQuestion :: PlayerId -> Int -> GameT Game
answerQuestion pid n = do
  g <- getGame
  question <- maybe (error "no question for player") pure $ Map.lookup pid g.question
  let choices = questionChoices question
  choice <- maybe (error "invalid choice") pure $ atMay choices n
  putGame $ g & questionL %~ Map.delete pid
  case question of
    ChooseOneAtATime _ | rest@(_ : _) <- take n choices <> drop (n + 1) choices -> do
      pushAll [uiToRun choice, Ask pid (ChooseOneAtATime rest)]
    _ -> push $ uiToRun choice
  mainLoop

-- | Run messages until the queue empties or someone has to answer a question.
mainLoop :: GameT Game
mainLoop = go (0 :: Int)
 where
  go n = do
    g <- getGame
    if
      | notNull g.question || isJust g.outcome -> pure g
      | n >= messageLimit -> error "message limit reached: the game is probably looping"
      | otherwise -> runNext >>= bool getGame (go (n + 1))

messageLimit :: Int
messageLimit = 10000

-- | Run the next queued message, returning False when the queue is empty.
runNext :: GameT Bool
runNext =
  popMessage >>= \case
    Nothing -> pure False
    Just (Ask pid q) -> do
      g <- getGame
      putGame $ g & questionL .~ Map.singleton pid q & questionVersionL +~ 1
      pure True
    Just msg -> do
      debug 1 $ print msg
      putGame =<< preloadModifiers =<< runMessage msg =<< getGame
      pure True

getPlayers :: (HasGame m) => m [Player]
getPlayers = toList <$> getsGame gamePlayers

instance RunMessage Entities where
  runMessage msg entities =
    traverseOf (heroesL . traverse) (runMessage msg) entities
      >>= traverseOf (alliesL . traverse) (runMessage msg)
      >>= traverseOf (locationsL . traverse) (runMessage msg)
      >>= traverseOf (enemiesL . traverse) (runMessage msg)
      >>= traverseOf (treacheriesL . traverse) (runMessage msg)
      >>= traverseOf (questsL . traverse) (runMessage msg)
      >>= traverseOf (attachmentsL . traverse) (runMessage msg)

instance RunMessage Game where
  runMessage msg g =
    traverseOf (playersL . traverse) (runMessage msg) g
      >>= traverseOf (activeCostsL . traverse) (runMessage msg)
      >>= traverseOf entitiesL (runMessage msg)
      >>= traverseOf scenarioL (runMessage msg)
      >>= runGameMessage msg

runGameMessage :: Message -> Game -> GameT Game
runGameMessage msg g = case msg of
  Run msgs -> do
    pushAll msgs
    pure g
  Setup -> do
    pushAll [DoStep n Setup | n <- [1 .. 7]]
    pure g
  DoStep 3 Setup -> do
    firstPlayer <- uniform g.playerOrder
    push $ SetFirstPlayer firstPlayer
    pure g
  BeginRound -> do
    pushAll [BeginPhase minBound, EndPhase minBound]
    pure $ g & roundL +~ 1
  EndRound -> do
    unless (isJust g.outcome) $ pushEnd BeginRound
    pure $ g & effectsL %~ filter ((/= UntilEndOfRound) . effectDuration)
  BeginPhase RefreshPhase -> do
    heroes <- select ExhaustedHero
    allies <- select ExhaustedAlly
    players <- getPlayerOrder
    let passToken = case drop 1 players of
          (next : _) -> [SetFirstPlayer next]
          [] -> []
    pushAll
      $ map (Ready . toTarget) heroes
      <> map (Ready . toTarget) allies
      <> [RaiseThreat pid 1 | pid <- players]
      <> passToken
    pure $ g & phaseL .~ RefreshPhase
  SetFirstPlayer pid -> pure $ g & firstPlayerL .~ pid
  CheckEliminations -> do
    eliminated <- forMaybeM (toList g.players) \player -> do
      heroes <- select (HeroControlledBy $ PlayerWithId player.id)
      pure $ guard (not player.eliminated && (player.threat >= eliminationThreat || null heroes)) $> player.id
    pushAll $ map EliminatePlayer eliminated
    pure g
  EliminatePlayer pid -> do
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    allies <- select (AllyControlledBy $ PlayerWithId pid)
    engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
    let remaining = [p | p <- toList g.players, p.id /= pid, not p.eliminated]
    pushAll
      $ map (Discard . toTarget) heroes
      <> map (Discard . toTarget) allies
      <> [PlaceEnemy eid StagingArea | eid <- engaged]
      <> [GameOver Lost | null remaining]
    pure g
  GameOver outcome -> do
    clearQueue
    pure $ g & outcomeL ?~ outcome
  Discard target -> discardTarget target g
  DealDamage source target n | n > 0 -> do
    pushAll
      [ PlaceTokens source target Damage n
      , CheckWindows [mkAfter $ DealtDamage target n]
      , CheckDefeated target
      ]
    pure g
  CheckDefeated target -> do
    destroyed <- case target of
      HeroTarget hid -> (>=) . (.damage) <$> getHero hid <*> getHitPoints target
      AllyTarget aid -> (>=) . (.damage) <$> getAlly aid <*> getHitPoints target
      EnemyTarget eid -> (>=) . (.damage) <$> getEnemy eid <*> getEnemyHitPoints eid
      _ -> pure False
    when destroyed $ push (Destroy target)
    pure g
  Destroy target@(EnemyTarget eid) -> do
    enemy <- getEnemy eid
    send $ Log $ display enemy.name <> " is destroyed"
    case cdVictoryPoints (toCardDef enemy) of
      Just _ -> do
        let card = MkEncounterCard (enemyCardId $ toAttrs enemy) (toCardCode enemy)
        pure
          $ g
          & entitiesL . enemiesL %~ Map.delete eid
          & scenarioL %~ overAttrs (victoryDisplayL %~ (card :))
      Nothing -> discardTarget target g
  Destroy target -> do
    pushAll [Discard target, CheckEliminations]
    pure g
  BeginPhase PlanningPhase -> do
    players <- getPlayers
    pushAll [WithTarget (toTarget player) (DoPhase PlanningPhase) | player <- players]
    pure $ g & phaseL .~ PlanningPhase
  BeginPhase QuestPhase -> do
    order <- getPlayerOrder
    pushAll $ map CommitCharacters order <> [Staging, QuestResolution]
    pure $ g & phaseL .~ QuestPhase
  BeginPhase TravelPhase -> do
    hasActive <- selectAny IsActiveLocation
    locations <- select LocationInStagingArea
    lead <- getLead
    unless (hasActive || null locations)
      $ push
      $ chooseOne lead
      $ [targetLabel lid [TravelTo lid] | lid <- locations]
      <> [Label "Do not travel" []]
    pure $ g & phaseL .~ TravelPhase
  TravelTo lid -> do
    pushAll [PlaceLocation lid ActiveLocation, CheckWindows [mkAfter $ Traveled lid]]
    pure g
  BeginPhase EncounterPhase -> do
    order <- getPlayerOrder
    pushAll $ map OptionalEngagement order <> [EngagementCheck pid 0 | pid <- take 1 order]
    pure $ g & phaseL .~ EncounterPhase
  OptionalEngagement pid -> do
    enemies <- select EnemyInStagingArea
    unless (null enemies)
      $ push
      $ chooseOne pid
      $ [targetLabel eid [EngageEnemy pid eid] | eid <- enemies]
      <> [Label "Do not engage" []]
    pure g
  EngagementCheck pid seatsPassed -> do
    order <- getPlayerOrder
    let next = seatAfter order pid
    player <- getPlayer pid
    candidates <- traverse (\eid -> (eid,) <$> getEngagementCost eid) =<< select EnemyInStagingArea
    case sortOn (Down . snd) [c | c@(_, cost) <- candidates, cost <= player.threat] of
      ((eid, _) : _) -> pushAll [EngageEnemy pid eid, EngagementCheck next 0]
      [] -> when (seatsPassed + 1 < length order) $ push (EngagementCheck next (seatsPassed + 1))
    pure g
  EngageEnemy pid eid -> do
    pushAll [PlaceEnemy eid (EngagedWith pid), CheckWindows [mkAfter $ EnemyEngaged eid pid]]
    pure g
  BeginPhase CombatPhase -> do
    order <- getPlayerOrder
    pushAll $ DealShadowCards : map EnemyAttacks order <> map PlayerAttacks order
    pure $ g & phaseL .~ CombatPhase
  DealShadowCards -> do
    order <- getPlayerOrder
    enemies <- concatMapM engagedByCost order
    pushAll $ map DealShadowCard enemies
    pure g
  EnemyAttacks pid -> do
    enemies <- engagedByCost pid
    pushAll $ map (EnemyAttack pid) enemies
    pure g
  EnemyAttack pid eid -> do
    defenders <- readyCharacters pid
    push
      $ chooseOne pid
      $ [targetLabel d [Exhaust d, DefendWith pid eid d] | d <- defenders]
      <> [Label "Take the attack undefended" [UndefendedAttack pid eid]]
    pure g
  DefendWith pid eid defender -> do
    pushAll [ResolveShadow eid pid (Just defender), AttackDamage eid defender]
    pure g
  UndefendedAttack pid eid -> do
    pushAll [ResolveShadow eid pid Nothing, UndefendedDamage pid eid]
    pure g
  UndefendedDamage pid eid -> do
    attack <- getEnemyAttack eid
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    unless (null heroes)
      $ push
      $ chooseOne pid [targetLabel h [DealDamage (EnemySource eid) (toTarget h) attack] | h <- heroes]
    pure g
  PlayerAttacks pid -> do
    enemies <- engagedByCost pid
    pushAll $ map (DeclareAttackers pid) enemies
    pure g
  DeclareAttackers pid eid -> do
    attackers <- readyCharacters pid
    push
      $ if null attackers
        then ResolveAttack pid eid
        else
          chooseOne pid
            $ [targetLabel a [AttackWith a, DeclareAttackers pid eid] | a <- attackers]
            <> [Label "Done attacking" [ResolveAttack pid eid]]
    pure g
  ResolveAttack pid eid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> AttackingHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> AttackingAlly)
    let attackers = map toTarget heroes <> map toTarget allies
    attack <- sum <$> traverse getAttack attackers
    defense <- getEnemyDefense eid
    pushAll $ map StopAttacking attackers <> [DealDamage (PlayerSource pid) (toTarget eid) (attack - defense) | notNull attackers]
    pure g
  AttackDamage eid defender -> do
    attack <- getEnemyAttack eid
    defense <- getDefense defender
    push $ DealDamage (EnemySource eid) defender (attack - defense)
    pure g
  ResolveShadow eid pid mDefender -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      for_ enemy.shadowCards \card -> resolveShadowEffect card (ResolveShadowEffect (ecId card) eid pid mDefender)
    pure g
  BeginPhase phase -> pure $ g & phaseL .~ phase
  CommitCharacters pid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly)
    let characters = map toTarget heroes <> map toTarget allies
    unless (null characters)
      $ push
      $ chooseOne pid
      $ [ targetLabel c [CommitToQuest c, CheckWindows [mkAfter $ CommittedToQuest c], CommitCharacters pid]
        | c <- characters
        ]
      <> [Label "Done committing" []]
    pure g
  Staging -> do
    pushAll [RevealEncounterCard | player <- toList g.players, not player.eliminated]
    pure g
  QuestResolution -> do
    heroes <- select CommittedHero
    allies <- select CommittedAlly
    willpower <- sum <$> traverse getWillpower (map toTarget heroes <> map toTarget allies)
    threat <- getStagingThreat
    players <- getPlayerOrder
    if
      | willpower > threat -> push $ PlaceProgressOnQuest (willpower - threat)
      | threat > willpower -> pushAll [RaiseThreat pid (threat - willpower) | pid <- players]
      | otherwise -> pure ()
    pure g
  PlaceProgressOnQuest n -> do
    active <- traverse getLocation =<< selectOne IsActiveLocation
    let onLocation = maybe 0 (\l -> min n (l.questPoints - l.progress)) active
        overflow = n - onLocation
    quests <- toList <$> getsGame (entitiesQuests . gameEntities)
    pushAll
      $ [PlaceTokens GameSource (toTarget l) Progress onLocation | l <- toList active, onLocation > 0]
      <> [PlaceTokens GameSource (toTarget q) Progress overflow | q <- quests, overflow > 0]
    pure g
  PayForCard pid card -> do
    costId <- getRandom
    push $ PayCost costId
    let activeCost =
          ActiveCost
            { activeCostId = costId
            , activeCostCost = cardCost card
            , activeCostCard = card
            , activeCostPlayer = pid
            }
    pure $ g & activeCostsL %~ insertEntity activeCost
  PaidCost costId -> pure $ g & activeCostsL %~ Map.delete costId
  CreateEffect effect -> pure $ g & effectsL %~ (effect :)
  EndPhase phase -> do
    pushEnd $ maybe EndRound (\next -> Run [BeginPhase next, EndPhase next]) (nextPhase phase)
    pure $ g & effectsL %~ filter ((/= UntilEndOfPhase) . effectDuration)
  PlayAlly pid card -> do
    player <- getPlayer pid
    thisAlly <- newAlly pid card
    send $ Log $ player.name <> " plays " <> display (cdName $ toCardDef card)
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  CheckWindows windows -> do
    abilities <- getWindowAbilities windows
    let forcedAbilities = filter isForcedAbility abilities
    lead <- getLead
    order <- getPlayerOrder
    forcedMsgs <- case forcedAbilities of
      [] -> pure []
      [ab] -> do
        controller <- fromMaybe lead <$> abilityController ab
        pure [UseAbility controller ab windows]
      abs' -> pure [Ask lead $ ChooseOneAtATime [AbilityLabel lead ab windows [] | ab <- abs']]
    pushAll $ forcedMsgs <> [ResponseWindow windows order [] 0]
    pure g
  ResponseWindow windows order used passes | passes < length order, (pid : _) <- order -> do
    abilities <- getWindowAbilities windows
    available <-
      filterM
        (\ab -> (== Just pid) <$> abilityController ab)
        [ab | ab <- abilities, isResponseAbility ab, abilityKey ab `notElem` used]
    let next = ResponseWindow windows (drop 1 order <> [pid]) used (passes + 1)
    push
      $ if null available
        then next
        else
          chooseOne pid
            $ [AbilityLabel pid ab windows [ResponseWindow windows order (abilityKey ab : used) 0] | ab <- available]
            <> [Label "Pass" [next]]
    pure g
  UseAbility pid ab windows -> do
    pushAll $ costMessages (PlayerSource pid) (abilityCost ab) <> [UseCardAbility pid ab.abilitySource ab.abilityIndex windows]
    pure g
  CompleteQuest qid -> do
    push AdvanceQuest
    pure $ g & entitiesL . questsL %~ Map.delete qid
  PlayAttachment pid card -> do
    attachment <- newAttachment pid card
    hosts <- attachmentHosts (toAttrs attachment).attachTo
    send . Log . (<> " plays " <> display (cdName $ toCardDef card)) . (.name) =<< getPlayer pid
    push $ chooseOrRunOne pid [targetLabel host [AttachCard (toId attachment) host, CheckWindows [mkAfter $ EntersPlay $ toTarget attachment]] | host <- hosts]
    pure $ g & entitiesL . attachmentsL %~ insertEntity attachment
  PutQuestIntoPlay card -> do
    thisQuest <- newQuest card
    pure $ g & entitiesL . questsL %~ insertEntity thisQuest
  RevealCard card -> do
    let def = toCardDef card
    send $ Log $ "Revealed " <> display (cdName def)
    let surge = [RevealEncounterCard | Surge `member` cdKeywords def]
    case cdCardType def of
      EnemyType -> do
        enemy <- newEnemy card
        pushAll $ WhenRevealed (toTarget enemy) : surge
        pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        pushAll $ WhenRevealed (toTarget location) : surge
        pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      TreacheryType -> do
        treachery <- newTreachery card
        pushAll $ [WhenRevealed (toTarget treachery), FinishRevelation (toId treachery)] <> surge
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      _ -> pure $ g & scenarioL %~ overAttrs (encounterDiscardL %~ (card :))
  FinishRevelation tid -> do
    -- a treachery that did not attach itself to something is discarded once it resolves
    for_ (Map.lookup tid g.entities.treacheries) \treachery ->
      when (treachery.placement == Unplaced) $ push (Discard $ toTarget treachery)
    pure g
  AddToStaging card -> case cdCardType (toCardDef card) of
    EnemyType -> do
      thisEnemy <- newEnemy card
      pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) thisEnemy)
    LocationType -> do
      thisLocation <- newLocation card
      pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) thisLocation)
    _ -> error "Unhandled"
  _ -> pure g

eliminationThreat :: Int
eliminationThreat = 50

-- | Remove an entity from play and put its card in the right discard pile.
discardTarget :: Target -> Game -> GameT Game
discardTarget target g = case target of
  HeroTarget hid -> fromEntity heroesL hid
  AllyTarget aid -> fromEntity alliesL aid
  EnemyTarget eid -> fromEntity enemiesL eid
  LocationTarget lid -> fromEntity locationsL lid
  TreacheryTarget tid -> fromEntity treacheriesL tid
  AttachmentTarget aid -> fromEntity attachmentsL aid
  _ -> pure g
 where
  fromEntity :: (Ord k, IsCard v) => Lens' Entities (Map k v) -> k -> GameT Game
  fromEntity l k = case Map.lookup k (g.entities ^. l) of
    Nothing -> pure g
    Just entity -> do
      -- attachments leave play with the card they are attached to
      pushAll [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
      pure $ addToDiscard (toCard entity) $ g & entitiesL . l %~ Map.delete k
  addToDiscard card = case card of
    PlayerCard pc | Just owner <- pcOwner pc -> playersL . ix owner . discardL %~ (card :)
    EncounterCard ec -> scenarioL %~ overAttrs (encounterDiscardL %~ (ec :))
    _ -> id

-- | The next player clockwise from the given one.
seatAfter :: [PlayerId] -> PlayerId -> PlayerId
seatAfter order pid = case dropWhile (/= pid) order of
  (_ : next : _) -> next
  _ -> fromMaybe pid (headMay order)

-- | Enemies engaged with a player, highest engagement cost first.
engagedByCost :: PlayerId -> GameT [EnemyId]
engagedByCost pid = do
  enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
  costs <- traverse getEngagementCost enemies
  pure $ map fst $ sortOn (Down . snd) $ zip enemies costs

readyCharacters :: PlayerId -> GameT [Target]
readyCharacters pid = do
  heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
  allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
  pure $ map toTarget heroes <> map toTarget allies

-- | Shadow cards are not in play, so the card's own module resolves the effect
-- through a temporary instance of it.
resolveShadowEffect :: EncounterCard -> Message -> GameT ()
resolveShadowEffect card msg = case cdCardType (toCardDef card) of
  EnemyType -> void . runMessage msg . createEnemy card =<< getRandom
  LocationType -> void . runMessage msg . createLocation card =<< getRandom
  TreacheryType -> void . runMessage msg . createTreachery card =<< getRandom
  _ -> pure ()

