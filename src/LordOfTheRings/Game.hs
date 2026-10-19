{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module X) where

import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally
import LordOfTheRings.Attachment
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy
import LordOfTheRings.Event
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
import LordOfTheRings.Json
import LordOfTheRings.Location
import LordOfTheRings.Matcher
import LordOfTheRings.Matcher.Window (windowMatches)
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (..), placementController)
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
newGame = newGameIn passageThroughMirkwood

newGameIn :: Scenario -> Player -> [Hero] -> Game
newGameIn gameScenario player heroes =
  Game
    { gamePlayers = Map.singleton player.id player
    , gamePlayerOrder = [player.id]
    , gameActivePlayer = player.id
    , gameFirstPlayer = player.id
    , gameEntities = mempty {entitiesHeroes = Map.fromList [(h.id, h) | h <- heroes]}
    , gameScenario
    , gamePhase = ResourcePhase
    , gameRound = 0
    , gameQuestion = mempty
    , gameQuestionVersion = 0
    , gameActiveCosts = mempty
    , gameModifiers = mempty
    , gameEffects = []
    , gameOutcome = Nothing
    , gameLastCommittedWillpower = Nothing
    }

answerQuestion :: PlayerId -> Int -> GameT Game
answerQuestion pid n = do
  g <- getGame
  question <- maybe (error "no question for player") pure $ Map.lookup pid g.question
  let choices = questionChoices question
  choice <- maybe (error "invalid choice") pure $ atMay choices n
  putGame $ g & questionL %~ Map.delete pid
  let rest = take n choices <> drop (n + 1) choices
      remaining = \case
        ChooseOneAtATime _ | notNull rest -> Just $ ChooseOneAtATime rest
        QuestionLabel l s ts q -> QuestionLabel l s ts <$> remaining q
        _ -> Nothing
  pushAll $ uiToRun choice : [Ask pid q | Just q <- [remaining question]]
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

-- | Run a debug message now, even while a question waits: it and whatever it leads to go
-- first, then the waiting question is asked again and play carries on from there.
runDebug :: Message -> GameT Game
runDebug msg = do
  asked <- getsGame (.question)
  waiting <- peekQueue
  putGame . (questionL .~ mempty) =<< getGame
  setQueue $ msg : [Ask pid q | (pid, q) <- Map.toList asked] <> waiting
  mainLoop

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
      -- store the result first, so the constant abilities read the game as it now is
      g <- runMessage msg =<< getGame
      putGame g
      putGame =<< preloadModifiers g
      pure True

getPlayers :: (HasGame m) => m [Player]
getPlayers = toList <$> getsGame gamePlayers

instance FromJSON Entities where
  parseJSON = genericParseJSON (aesonOptions $ Just "entities")

instance FromJSON Game where
  parseJSON = genericParseJSON (aesonOptions $ Just "game")

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
    logMessage msg
      >> traverseOf (playersL . traverse) (runMessage msg) g
      >>= traverseOf (activeCostsL . traverse) (runMessage msg)
      >>= traverseOf entitiesL (runMessage msg)
      >>= traverseOf scenarioL (runMessage msg)
      >>= runGameMessage msg
      >>= \g' -> case msg of
        -- the window goes in front of the phase's own steps
        BeginPhase phase -> g' <$ push (CheckWindows [mkAfter $ PhaseBegins phase])
        _ -> pure g'

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
    heroes <- filterM (fmap (notElem DoesNotReadyDuringRefresh) . getModifiers) =<< select ExhaustedHero
    allies <- select ExhaustedAlly
    players <- getPlayerOrder
    let passToken = case drop 1 players of
          (next : _) -> [SetFirstPlayer next]
          [] -> []
    pushAll
      $ map (Ready . toTarget) heroes
      <> map (Ready . toTarget) allies
      <> [Ready (toTarget a) | a <- toList g.entities.attachments, attachmentExhausted (toAttrs a)]
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
  HealDamage source target n | n > 0 -> do
    damage <- case target of
      HeroTarget hid -> (.damage) <$> getHero hid
      AllyTarget aid -> (.damage) <$> getAlly aid
      _ -> pure 0
    pushWhen (damage > 0) $ RemoveTokens source target Damage (min n damage)
    pure g
  ChooseAndDiscardFromHand pid source n | n > 0 -> do
    player <- getPlayer pid
    unless (null player.hand)
      $ push
      $ questionLabelWith "Choose a card to discard from your hand" (sourceTarget source) [] pid
        [targetLabel (CardIdTarget c.id) [DiscardFromHand pid c.id, ChooseAndDiscardFromHand pid source (n - 1)] | c <- player.hand]
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
    pushAll [Discard target, CheckEliminations, CheckWindows [mkAfter $ CharacterDestroyed target]]
    pure g
  BeginPhase PlanningPhase -> do
    players <- getPlayers
    pushAll [WithTarget (toTarget player) (DoPhase PlanningPhase) | player <- players]
    pure $ g & phaseL .~ PlanningPhase
  BeginPhase QuestPhase -> do
    order <- getPlayerOrder
    pushAll $ map CommitCharacters order <> [Staging] <> map ActionWindow order <> [QuestResolution]
    pure $ g & phaseL .~ QuestPhase
  BeginPhase TravelPhase -> do
    hasActive <- selectAny IsActiveLocation
    locations <- filterM (fmap (notElem CannotBeTraveledTo) . getModifiers) =<< select LocationInStagingArea
    lead <- getLead
    forced <- filterM (fmap (elem MustBeTraveledTo) . getModifiers) locations
    unless (hasActive || null locations)
      $ push
      $ if null forced
        then
          questionLabel "Travel to a location?" lead
            $ [targetLabel lid [TravelTo lid] | lid <- locations]
            <> [Label "Do not travel" []]
        else questionLabel "You must travel to a location" lead [targetLabel lid [TravelTo lid] | lid <- forced]
    pure $ g & phaseL .~ TravelPhase
  TravelTo lid -> do
    pushAll [PlaceLocation lid ActiveLocation, CheckWindows [mkAfter $ Traveled lid]]
    pure g
  BeginPhase EncounterPhase -> do
    order <- getPlayerOrder
    pushAll $ map OptionalEngagement order <> [EngagementCheck pid 0 | pid <- take 1 order]
    pure $ g & phaseL .~ EncounterPhase
  OptionalEngagement pid -> do
    staged <- select EnemyInStagingArea
    -- some enemies may only be engaged once they are alone in the staging area
    enemies <- flip filterM staged \eid ->
      if length staged > 1 then notElem CannotBeOptionallyEngagedWhileOthersInStaging <$> getModifiers eid else pure True
    unless (null enemies)
      $ push
      $ questionLabel "Engage an enemy from the staging area?" pid
      $ [targetLabel eid [EngageEnemy pid eid] | eid <- enemies]
      <> [Label "Do not engage" []]
    pure g
  EngagementCheck pid seatsPassed -> do
    skip <- elem NoEngagementChecks <$> getModifiers GameTarget
    order <- getPlayerOrder
    let next = seatAfter order pid
    player <- getPlayer pid
    candidates <- traverse (\eid -> (eid,) <$> getEngagementCost eid) =<< select EnemyInStagingArea
    case sortOn (Down . snd) [c | not skip, c@(_, cost) <- candidates, cost <= player.threat] of
      ((eid, _) : _) -> pushAll [EngageEnemy pid eid, EngagementCheck next 0]
      [] -> when (seatsPassed + 1 < length order) $ push (EngagementCheck next (seatsPassed + 1))
    pure g
  EngageEnemy pid eid -> do
    pushAll [PlaceEnemy eid (EngagedWith pid), CheckWindows [mkAfter $ EnemyEngaged eid pid]]
    pure g
  BeginPhase CombatPhase -> do
    order <- getPlayerOrder
    pushAll $ DealShadowCards : map ActionWindow order <> map EnemyAttacks order <> map PlayerAttacks order
    pure $ g & phaseL .~ CombatPhase
  DealShadowCards -> do
    order <- getPlayerOrder
    enemies <- concatMapM engagedByCost order
    pushAll $ map DealShadowCard enemies
    pure g
  EnemyAttacks pid -> do
    shielded <- elem EngagedEnemiesCannotAttack <$> getModifiers (PlayerTarget pid)
    enemies <- filterM (fmap (notElem CannotAttack) . getModifiers) =<< engagedByCost pid
    unless shielded $ pushAll $ map (EnemyAttack pid) enemies
    pure g
  EnemyAttack pid eid -> do
    many <- elem MayDeclareMultipleDefenders <$> getModifiers (PlayerTarget pid)
    defenders <- readyCharacters pid
    enemyName <- display . (.name) <$> getEnemy eid
    let declare =
          if many
            then DeclareDefenders pid eid []
            else
              questionLabelWith (enemyName <> " attacks: declare a defender") (Just $ toTarget eid) [] pid
                $ [targetLabel d [Exhaust d, DefendWith pid eid d] | d <- defenders]
                <> [Label "Take the attack undefended" [UndefendedAttack pid eid]]
    pushAll [CheckWindows [mkWhen $ EnemyAttacking eid pid], declare]
    pure g
  DefendWith pid eid defender -> do
    pushAll
      [ CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
      , ResolveShadow eid pid (Just defender)
      , AttackDamage eid defender
      , CheckWindows [mkAfter $ EnemyAttacking eid pid]
      ]
    pure g
  DeclareDefenders pid eid chosen -> do
    defenders <- readyCharacters pid
    enemyName <- display . (.name) <$> getEnemy eid
    push
      $ questionLabelWith (enemyName <> " attacks: declare any number of defenders") (Just $ toTarget eid) chosen pid
      $ [ targetLabel d [Exhaust d, CheckWindows [mkAfter $ DeclaredAsDefender d eid], DeclareDefenders pid eid (chosen <> [d])]
        | d <- defenders
        ]
      <> case chosen of
        [] -> [Label "Take the attack undefended" [UndefendedAttack pid eid]]
        first : _ ->
          [ Label "Done declaring defenders"
              [ResolveShadow eid pid (Just first), DefendedByMany pid eid chosen, CheckWindows [mkAfter $ EnemyAttacking eid pid]]
          ]
    pure g
  DefendedByMany pid eid defenders -> do
    attack <- getEnemyAttack eid
    defense <- sum <$> traverse getDefense defenders
    choices <- for defenders \d -> targetLabel d <$> attackDamage pid eid d (attack - defense)
    push $ chooseOrRunOne pid choices
    pure g
  UndefendedAttack pid eid -> do
    pushAll [ResolveShadow eid pid Nothing, UndefendedDamage pid eid, CheckWindows [mkAfter $ EnemyAttacking eid pid]]
    pure g
  UndefendedDamage pid eid -> do
    attack <- getEnemyAttack eid
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    choices <- for heroes \h -> targetLabel h <$> attackDamage pid eid (toTarget h) attack
    unless (null heroes)
      $ push
      $ questionLabelWith "The attack is undefended: which hero takes the damage?" (Just $ toTarget eid) [] pid choices
    pure g
  PlayerAttacks pid -> do
    enemies <- engagedByCost pid
    loners <- filterM (fmap (elem CanAttackStagingAreaAlone) . getModifiers) =<< select (HeroControlledBy $ PlayerWithId pid)
    pushAll $ map (DeclareAttackers pid) enemies <> [AttackStagingAreaAlone pid (toTarget h) | h <- loners]
    pure g
  AttackStagingAreaAlone pid attacker -> do
    ready <- elem attacker <$> readyCharacters pid
    staged <- select EnemyInStagingArea
    when (ready && notNull staged)
      $ push
      $ questionLabelWith "Attack an enemy in the staging area alone?" (Just attacker) [] pid
      $ [ targetLabel eid [AttackWith attacker, CreateEffect $ Effect GameSource attacker [AttackModifier 1] UntilEndOfPhase, ResolveAttack pid eid]
        | eid <- staged
        ]
      <> [Label "Do not attack" []]
    pure g
  DeclareAttackers pid eid -> do
    attackers <- readyCharacters pid
    push
      $ if null attackers
        then ResolveAttack pid eid
        else
          questionLabelWith "Declare attackers" Nothing [toTarget eid] pid
            $ [targetLabel a [AttackWith a, DeclareAttackers pid eid] | a <- attackers]
            <> [Label "Done attacking" [ResolveAttack pid eid]]
    pure g
  ResolveAttack pid eid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> AttackingHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> AttackingAlly)
    let attackers = map toTarget heroes <> map toTarget allies
    enemyDef <- toCardDef <$> getEnemy eid
    bonuses <- for attackers \a -> do
      mods <- getModifiers a
      sum <$> for [(trait, n) | AttackBonusAgainst trait n <- mods] \(trait, n) -> bool 0 n <$> hasTrait trait (toTarget eid) enemyDef
    attack <- (+ sum bonuses) . sum <$> traverse getAttack attackers
    defense <- getEnemyDefense eid
    pushAll
      $ map StopAttacking attackers
      <> [DealDamage (PlayerSource pid) (toTarget eid) (attack - defense) | notNull attackers]
      <> [AttackResolved eid attackers | notNull attackers]
    pure g
  AttackResolved eid attackers -> do
    destroyed <- not <$> selectAny (EnemyWithId eid)
    pushWhen destroyed $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid attackers]
    pure g
  AttackDamage eid defender -> do
    attack <- getEnemyAttack eid
    defense <- getDefense defender
    engaged <- placementController . (.placement) <$> getEnemy eid
    lead <- getLead
    pushAll =<< attackDamage (fromMaybe lead engaged) eid defender (attack - defense)
    pure g
  ResolveShadow eid pid mDefender -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      pushAll
        $ concat
          [ [CheckWindows [mkWhen $ ShadowEffectTriggered (ecId card) eid], ResolveShadowCard (ecId card) eid pid mDefender]
          | card <- enemy.shadowCards
          , cdShadowEffect (toCardDef card)
          ]
    pure g
  ResolveShadowCard cid eid pid mDefender -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      for_ (find ((== cid) . ecId) enemy.shadowCards) \card ->
        resolveShadowEffect card (ResolveShadowEffect cid eid pid mDefender)
    pure g
  ChooseAndExhaust source pid n | n > 0 -> do
    ready <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
    unless (null ready)
      $ push
      $ questionLabelWith "Choose and exhaust a character" (sourceTarget source) [] pid
        [targetLabel c [Exhaust c, ChooseAndExhaust source pid (n - 1)] | c <- ready]
    pure g
  ChooseAndExhaustHero source pid -> do
    ready <- map toTarget <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
    unless (null ready)
      $ push
      $ questionLabelWith "Choose and exhaust a hero" (sourceTarget source) [] pid [targetLabel h [Exhaust h] | h <- ready]
    pure g
  ChooseAndDiscard source pid targets -> do
    unless (null targets)
      $ push
      $ questionLabelWith "Choose a card to discard" (sourceTarget source) [] pid [targetLabel t [Discard t] | t <- targets]
    pure g
  BeginPhase phase -> pure $ g & phaseL .~ phase
  CommitCharacters pid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly)
    let characters = map toTarget heroes <> map toTarget allies
    quests <- getsGame (map toTarget . toList . entitiesQuests . gameEntities)
    unless (null characters)
      $ push
      $ questionLabelWith "Commit characters to the quest" Nothing quests pid
      $ [ targetLabel c [CommitToQuest c, CheckWindows [mkAfter $ CommittedToQuest c], CommitCharacters pid]
        | c <- characters
        ]
      <> [Label "Done committing" []]
    pure g
  Staging -> do
    mods <- getModifiers GameTarget
    let extra = sum [n | AdditionalStagingReveals n <- mods]
        players = length [() | player <- toList g.players, not player.eliminated]
    unless (SkipStagingStep `elem` mods) $ pushAll $ replicate (players + extra) RevealEncounterCard
    pure g
  QuestResolution -> do
    willpower <- getCommittedWillpower
    threat <- getStagingThreat
    players <- getPlayerOrder
    if
      | willpower > threat -> push $ PlaceProgressOnQuest (willpower - threat)
      | threat > willpower -> pushAll [RaiseThreat pid (threat - willpower) | pid <- players]
      | otherwise -> pure ()
    pure $ g & lastCommittedWillpowerL ?~ willpower
  PlaceProgressOnQuest n -> do
    active <- traverse getLocation =<< selectOne IsActiveLocation
    let onLocation = maybe 0 (\l -> min n (l.questPoints - l.progress)) active
        overflow = n - onLocation
    quests <- toList <$> getsGame (entitiesQuests . gameEntities)
    pushAll
      $ [PlaceTokens GameSource (toTarget l) Progress onLocation | l <- toList active, onLocation > 0]
      <> [PlaceTokens GameSource (toTarget q) Progress overflow | q <- quests, overflow > 0]
    pure g
  PayForCard pid card windows -> do
    costId <- getRandom
    push $ PayCost costId
    let activeCost =
          ActiveCost
            { activeCostId = costId
            , activeCostCost = cardCost card
            , activeCostCard = card
            , activeCostPlayer = pid
            , activeCostWindows = windows
            }
    pure $ g & activeCostsL %~ insertEntity activeCost
  PaidCost costId -> pure $ g & activeCostsL %~ Map.delete costId
  CreateEffect effect -> pure $ g & effectsL %~ (effect :)
  EndPhase phase -> do
    push $ CheckWindows [mkWhen $ PhaseEnds phase]
    when (phase == CombatPhase) do
      returning <- filterM (fmap (elem ReturnToTopOfEncounterDeckAfterCombat) . getModifiers) (toList g.entities.enemies)
      pushAll [ReturnToTopOfEncounterDeck (toTarget e) | e <- returning]
    returning <- flip filterM (toList g.entities.allies) \a ->
      elem ReturnToHandAtEndOfPhase <$> getModifiers (CardIdTarget $ allyCardId $ toAttrs a)
    shuffling <- flip filterM (toList g.entities.allies) \a -> elem ShuffleIntoDeckAtEndOfPhase <$> getModifiers a
    pushAll $ [ReturnToHand (toTarget a) | a <- returning] <> [ShuffleIntoOwnersDeck (toTarget a) | a <- shuffling]
    pushEnd $ maybe EndRound (\next -> Run [BeginPhase next, EndPhase next]) (nextPhase phase)
    pure $ g & effectsL %~ filter ((/= UntilEndOfPhase) . effectDuration)
  PlayAlly pid card -> do
    player <- getPlayer pid
    thisAlly <- newAlly pid card
    send $ Log $ player.name <> " plays " <> display (cdName $ toCardDef card)
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutAllyIntoPlayUnder pid card -> do
    thisAlly <- newAlly (fromMaybe pid card.owner) card
    send $ Log $ display (cdName $ toCardDef card) <> " is put into play"
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutHeroIntoPlay pid card -> do
    thisHero <- newHero pid card
    send $ Log $ display (cdName $ toCardDef card) <> " is put into play"
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisHero]
    pure $ g & entitiesL . heroesL %~ insertEntity thisHero
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
    fromHand <- handResponses pid windows (ResponseWindow windows order used 0)
    let next = ResponseWindow windows (drop 1 order <> [pid]) used (passes + 1)
    push
      $ if null available && null fromHand
        then next
        else
          questionLabel "Use a response?" pid
            $ [AbilityLabel pid ab windows [ResponseWindow windows order (abilityKey ab : used) 0] | ab <- available]
            <> fromHand
            <> [Label "Pass" [next]]
    pure g
  UseAbility pid ab windows -> do
    pushAll $ costMessages (PlayerSource pid) (abilityCost ab) <> [UseCardAbility pid ab.abilitySource ab.abilityIndex windows]
    pure g
  PlayEvent pid card _ -> do
    player <- getPlayer pid
    send $ Log $ player.name <> " plays " <> display (cdName $ toCardDef card)
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runMessage msg
    pure g
  ReturnToHand target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    leftPlay g target
    pushAll [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    send $ Log $ display a.name <> " returns to its owner's hand"
    pure $ g & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . handL %~ (<> [toCard a])
  ShuffleIntoOwnersDeck target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    leftPlay g target
    pushAll [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    send $ Log $ display a.name <> " is shuffled into its owner's deck"
    deck <- shuffleM . (toCard a :) . (.deck) =<< getPlayer a.owner
    pure $ g & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . deckL .~ deck
  ReturnToTopOfEncounterDeck target -> do
    let toTop card = scenarioL %~ overAttrs (encounterDeckL %~ (card :))
        withAttached = pushAll [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
    case target of
      EnemyTarget eid | Just e <- Map.lookup eid g.entities.enemies -> do
        withAttached
        pure $ g & entitiesL . enemiesL %~ Map.delete eid & toTop (MkEncounterCard (enemyCardId $ toAttrs e) (toCardCode e))
      LocationTarget lid | Just l <- Map.lookup lid g.entities.locations -> do
        withAttached
        pure $ g & entitiesL . locationsL %~ Map.delete lid & toTop (MkEncounterCard (locationCardId $ toAttrs l) (toCardCode l))
      _ -> pure g
  RemoveProgressFromQuest source n -> do
    quests <- toList <$> getsGame (entitiesQuests . gameEntities)
    pushAll [RemoveTokens source (toTarget q) Progress (min n q.progress) | q <- quests, q.progress > 0]
    pure g
  PutShadowCardIntoPlay host cid pid -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . (.id)) hostEnemy.shadowCards) \card -> do
        attacker <- newEnemy card
        pushAll
          [ RemoveShadowCard host cid
          , PlaceEnemy attacker.id (EngagedWith pid)
          , CreateEffect $ Effect (toSource attacker) (toTarget attacker) [ReturnToTopOfEncounterDeckAfterCombat] UntilEndOfRound
          , DealShadowCard attacker.id
          , EnemyAttack pid attacker.id
          ]
        putGame $ g & entitiesL . enemiesL %~ insertEntity attacker
    getGame
  TakeControl aid pid -> pure $ g & entitiesL . alliesL . ix aid %~ overAttrs (\a -> a {allyPlacement = InPlayArea pid})
  CompleteQuest qid -> do
    push AdvanceQuest
    pure $ g & entitiesL . questsL %~ Map.delete qid
  PlayAttachment pid card -> do
    attachment <- newAttachment pid card
    hosts <- attachmentHosts (toAttrs attachment).attachTo
    send . Log . (<> " plays " <> display (cdName $ toCardDef card)) . (.name) =<< getPlayer pid
    let attachTo host = targetLabel host [AttachCard (toId attachment) host, CheckWindows [mkAfter $ EntersPlay $ toTarget attachment]]
    push $ case hosts of
      [host] -> uiToRun (attachTo host)
      _ -> questionLabelWith "Attach to" (Just $ toTarget attachment) hosts pid (map attachTo hosts)
    pure $ g & entitiesL . attachmentsL %~ insertEntity attachment
  PutQuestIntoPlay card -> do
    thisQuest <- newQuest card
    pure $ g & entitiesL . questsL %~ insertEntity thisQuest
  RevealCard card -> do
    let def = toCardDef card
    send $ Log $ "Revealed " <> display (cdName def)
    players <- getPlayerOrder
    let surge = [RevealEncounterCard | Surge `member` cdKeywords def]
        doomed = [RaiseThreat p n | Doomed n <- toList (cdKeywords def), p <- players]
        revealing t = [CheckWindows [mkWhen $ Revealed t], WhenRevealed t] <> doomed <> [CheckWindows [mkAfter $ Revealed t]]
    case cdCardType def of
      EnemyType -> do
        enemy <- newEnemy card
        pushAll $ revealing (toTarget enemy) <> surge
        pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        pushAll $ revealing (toTarget location) <> surge
        pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      TreacheryType -> do
        treachery <- newTreachery card
        pushAll $ revealing (toTarget treachery) <> [FinishRevelation (toId treachery)] <> surge
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

-- | Tell the players what just happened, before it happens.
logMessage :: Message -> GameT ()
logMessage = \case
  BeginRound -> do
    n <- getsGame ((+ 1) . gameRound)
    send $ Log $ "Round " <> tshow n
  BeginPhase phase -> do
    let label = phaseName phase
    send $ Log label
    send $ Announce label
  RaiseThreat pid n -> do
    player <- getPlayer pid
    send $ Log $ player.name <> " threat +" <> tshow n
  LowerThreat pid n -> do
    player <- getPlayer pid
    send $ Log $ player.name <> " threat -" <> tshow n
  EngageEnemy pid eid -> do
    player <- getPlayer pid
    enemy <- getEnemy eid
    send $ Log $ display enemy.name <> " engages " <> player.name
  PlaceTokens _ target Progress n -> whenJustM (targetName target) \name -> send $ Log $ tshow n <> " progress on " <> name
  DealDamage _ target n | n > 0 -> whenJustM (targetName target) \name -> send $ Log $ name <> " takes " <> tshow n <> " damage"
  Discard target -> whenJustM (targetName target) \name -> send $ Log $ name <> " leaves play"
  AdvanceToQuest card -> send $ FlipCard card.id
  GameOver outcome -> do
    send $ Log $ "Game over: " <> tshow outcome
    send $ Announce $ case outcome of
      Won -> "The players win"
      Lost -> "The players lose"
  _ -> pure ()

phaseName :: Phase -> Text
phaseName = \case
  ResourcePhase -> "Resource phase"
  PlanningPhase -> "Planning phase"
  QuestPhase -> "Quest phase"
  TravelPhase -> "Travel phase"
  EncounterPhase -> "Encounter phase"
  CombatPhase -> "Combat phase"
  RefreshPhase -> "Refresh phase"

targetName :: Target -> GameT (Maybe Text)
targetName target = do
  es <- getsGame gameEntities
  pure $ display <$> case target of
    HeroTarget hid -> (.name) <$> Map.lookup hid es.heroes
    AllyTarget aid -> (.name) <$> Map.lookup aid es.allies
    EnemyTarget eid -> (.name) <$> Map.lookup eid es.enemies
    LocationTarget lid -> (.name) <$> Map.lookup lid es.locations
    AttachmentTarget aid -> (.name) <$> Map.lookup aid es.attachments
    QuestTarget qid -> (.name) <$> Map.lookup qid es.quests
    TreacheryTarget tid -> (.name) <$> Map.lookup tid es.treacheries
    _ -> Nothing

eliminationThreat :: Int
eliminationThreat = 50

-- | Remove an entity from play and put its card in the right discard pile.
discardTarget :: Target -> Game -> GameT Game
discardTarget target g = leftPlay g target >> case target of
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
      -- attachments, and encounter cards attached as conditions, leave play with their host
      pushAll
        $ [Discard (toTarget a) | a <- toList g.entities.attachments, a.placement == AttachedTo target]
        <> [Discard (toTarget t) | t <- toList g.entities.treacheries, t.placement == AttachedTo target]
      mods <- getModifiers target
      let removed = g & entitiesL . l %~ Map.delete k
      case toCard entity of
        EncounterCard ec
          | LeavesPlayToTopOfEncounterDeck `elem` mods -> pure $ removed & scenarioL %~ overAttrs (encounterDeckL %~ (ec :))
          | ShuffledIntoEncounterDeckWhenDefeated `elem` mods -> do
              deck <- shuffleM (ec : (toAttrs g.scenario).encounterDeck)
              pure $ removed & scenarioL %~ overAttrs (encounterDeckL .~ deck)
        card -> pure $ addToDiscard card removed
  addToDiscard card = case card of
    PlayerCard pc | Just owner <- pcOwner pc -> playersL . ix owner . discardL %~ (card :)
    EncounterCard ec -> scenarioL %~ overAttrs (encounterDiscardL %~ (ec :))
    _ -> id

-- | Open the window for a character leaving play, while it can still be looked at.
leftPlay :: Game -> Target -> GameT ()
leftPlay g target = case target of
  HeroTarget hid | Just h <- Map.lookup hid g.entities.heroes -> window h.owner (toCardDef h)
  AllyTarget aid | Just a <- Map.lookup aid g.entities.allies, Just pid <- placementController a.placement -> window pid (toCardDef a)
  _ -> pure ()
 where
  window pid def = do
    gained <- mapMaybe (\case GainTrait t -> Just t; _ -> Nothing) <$> getModifiers target
    push $ CheckWindows [mkAfter $ LeftPlay target pid (cdCardTraits def <> setFromList gained)]

-- | Cards in a player's hand that can be played or put into play in response to the
-- windows: response events, and allies like Brok Ironfist.
handResponses :: PlayerId -> [Window] -> Message -> GameT [UI Message]
handResponses pid windows continue = do
  player <- getPlayer pid
  let matches = maybe False (\w -> any (windowMatches w) windows)
  fmap catMaybes $ for player.hand \case
    card@(PlayerCard pc) -> case cdCardType (toCardDef pc) of
      EventType
        | Just e <- lookupEvent (toCardCode pc) pc.id pid
        , matches (toAttrs e).eventWindow -> do
            ok <- andM [passesCriteria (toAttrs e).eventCriteria, canAfford player card]
            pure $ guard ok $> targetLabel (CardIdTarget pc.id) [PlayCardInWindow pid pc.id windows, continue]
      AllyType
        | matches (allyHandWindow $ toAttrs $ createAlly pc (AllyId UUID.nil) pid) -> do
            ok <- passesUnique card
            pure $ guard ok $> targetLabel (CardIdTarget pc.id) [PutAllyIntoPlay pid pc.id, continue]
      _ -> pure Nothing
    EncounterCard _ -> pure Nothing

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


-- | The damage an enemy's attack deals to a character, and, for an enemy whose excess
-- damage counts against the player, the threat it raises.
attackDamage :: PlayerId -> EnemyId -> Target -> Int -> GameT [Message]
attackDamage pid eid target n = do
  excessToThreat <- elem ExcessDamageToThreat <$> getModifiers eid
  remaining <- (-) <$> getHitPoints target <*> getDamage target
  let excess = if excessToThreat then max 0 (n - remaining) else 0
  pure $ DealDamage (EnemySource eid) target n : [RaiseThreat pid excess | excess > 0]
