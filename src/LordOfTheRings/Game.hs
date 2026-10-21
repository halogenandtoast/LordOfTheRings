{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Game (module LordOfTheRings.Game, module X) where

import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import LordOfTheRings.ActiveCost
import LordOfTheRings.Ally
import LordOfTheRings.Attachment
import LordOfTheRings.Card
import LordOfTheRings.Card.Synthetic (orcGuard)
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
import LordOfTheRings.Log.Narrator
import LordOfTheRings.Log.Block
import LordOfTheRings.Log.Refs (phaseKey, sendLogInOpenBlock)
import LordOfTheRings.Objective
import LordOfTheRings.Matcher
import LordOfTheRings.Matcher.Window (windowMatches)
import LordOfTheRings.Message
import LordOfTheRings.Modifier
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement (Placement (..), placementController)
import LordOfTheRings.Player.Runner as X
import LordOfTheRings.I18n
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
newGameIn gameScenario player heroes = newTableIn gameScenario ((player, heroes) :| [])

-- | A game with a seat for each player, in the order given.
newTableIn :: Scenario -> NonEmpty (Player, [Hero]) -> Game
newTableIn gameScenario seats@((player, _) :| _) =
  Game
    { gamePlayers = Map.fromList [(p.id, p) | (p, _) <- toList seats]
    , gamePlayerOrder = [p.id | (p, _) <- toList seats]
    , gameActivePlayer = player.id
    , gameFirstPlayer = player.id
    , gameEntities = mempty {entitiesHeroes = Map.fromList [(h.id, h) | (_, hs) <- toList seats, h <- hs]}
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
    , gameLogBlocks = []
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
-- first, then the waiting question is asked again (a window is built afresh, as what it
-- offers may have changed) and play carries on from there.
runDebug :: Message -> GameT Game
runDebug msg = do
  asked <- getsGame (.question)
  waiting <- peekQueue
  putGame . (questionL .~ mempty) =<< getGame
  setQueue $ msg : [fromMaybe (Ask pid q) (questionRebuild q) | (pid, q) <- Map.toList asked] <> waiting
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
      -- the log, read off the game as the message finds it
      for_ (narrationFor msg) \build -> traverse_ sendLogInOpenBlock =<< build
      -- store the result first, so the constant abilities read the game as it now is
      g <- runGame msg =<< getGame
      putGame g
      putGame =<< preloadModifiers g
      pure True

getPlayers :: (HasGame m) => m [Player]
getPlayers = toList <$> getsGame gamePlayers

instance FromJSON Entities where
  parseJSON = genericParseJSON (aesonOptions $ Just "entities") . withDefaults [("objectives", object [])]

instance FromJSON Game where
  parseJSON = genericParseJSON (aesonOptions $ Just "game") . withDefaults [("logBlocks", toJSON ([] :: [Value]))]

-- | Run the message on one thing in play, putting what it pushes on the queue once it is
-- done.
runEach :: (RunMessage a) => Message -> a -> GameT a
runEach msg = runQueueT . runMessage msg

runEntities :: Message -> Entities -> GameT Entities
runEntities msg entities =
  traverseOf (heroesL . traverse) (runEach msg) entities
    >>= traverseOf (alliesL . traverse) (runEach msg)
    >>= traverseOf (locationsL . traverse) (runEach msg)
    >>= traverseOf (enemiesL . traverse) (runEach msg)
    >>= traverseOf (treacheriesL . traverse) (runEach msg)
    >>= traverseOf (questsL . traverse) (runEach msg)
    >>= traverseOf (attachmentsL . traverse) (runEach msg)
    >>= traverseOf (objectivesL . traverse) (runEach msg)

runGame :: Message -> Game -> GameT Game
runGame msg g =
  announce msg
    >> traverseOf (playersL . traverse) (runEach msg) g
    >>= traverseOf (activeCostsL . traverse) (runEach msg)
    >>= traverseOf entitiesL (runEntities msg)
    >>= traverseOf scenarioL (runEach msg)
    >>= runGameMessage msg
    >>= \g' -> case msg of
      -- the window goes in front of the phase's own steps
      BeginPhase phase -> g' <$ push (CheckWindows [mkAfter $ PhaseBegins phase])
      _ -> pure g'

runGameMessage :: Message -> Game -> GameT Game
runGameMessage msg g = case msg of
  -- the rest of an attack does nothing once its enemy has left play
  _ | Just eid <- attackingEnemy msg, not (Map.member eid g.entities.enemies) -> pure g
  Run msgs -> do
    pushAll msgs
    pure g
  OpenLogBlock block -> pure $ g & logBlocksL %~ (<> [block])
  CloseLogBlock bid -> pure $ g & logBlocksL %~ filter ((/= bid) . (.id))
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
    unless (isJust g.outcome) do
      push $ CheckWindows [mkWhen RoundEnds]
      pushEnd BeginRound
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
    damage <- getDamage target
    pushWhen (damage > 0) $ RemoveTokens source target Damage (min n damage)
    pure g
  ChooseAndDiscardFromHand pid source n | n > 0 -> do
    player <- getPlayer pid
    unless (null player.hand)
      $ push
      $ questionLabelWith (ikey "label.chooseCardToDiscardFromHand") (sourceTarget source) [] pid
        [targetLabel (CardIdTarget c.id) [DiscardFromHand pid c.id, ChooseAndDiscardFromHand pid source (n - 1)] | c <- player.hand]
    pure g
  CheckDefeated target -> do
    -- damage dealt in a batch can reach a card an earlier part of it already destroyed
    destroyed <- case target of
      HeroTarget hid | Map.member hid g.entities.heroes -> (>=) . (.damage) <$> getHero hid <*> getHitPoints target
      AllyTarget aid | Map.member aid g.entities.allies -> (>=) . (.damage) <$> getAlly aid <*> getHitPoints target
      EnemyTarget eid | Map.member eid g.entities.enemies -> (>=) . (.damage) <$> getEnemy eid <*> getEnemyHitPoints eid
      _ -> pure False
    when destroyed $ push (Destroy target)
    pure g
  Destroy target@(EnemyTarget eid) -> do
    enemy <- getEnemy eid
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
          questionLabel (ikey "label.travelToLocation") lead
            $ [targetLabel lid [TravelTo lid] | lid <- locations]
            <> [Label (ikey "label.doNotTravel") []]
        else questionLabel (ikey "label.mustTravel") lead [targetLabel lid [TravelTo lid] | lid <- forced]
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
      $ questionLabel (ikey "label.optionalEngagement") pid
      $ [targetLabel eid [EngageEnemy pid eid] | eid <- enemies]
      <> [Label (ikey "label.doNotEngage") []]
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
    push . ChooseEnemyAttack pid =<< engagedByCost pid
    pure g
  ChooseEnemyAttack pid candidates -> do
    -- checked again before each attack: a card played between attacks may stop one
    shielded <- elem EngagedEnemiesCannotAttack <$> getModifiers (PlayerTarget pid)
    engaged <- select (EnemyEngagedWith $ PlayerWithId pid)
    enemies <- filterM (fmap (notElem CannotAttack) . getModifiers) [e | e <- candidates, e `elem` engaged]
    unless shielded $ case enemies of
      [] -> pure ()
      [eid] -> push $ EnemyAttack pid eid
      _ ->
        push
          $ questionLabelWith (ikey "label.chooseNextAttack") Nothing (map toTarget enemies) pid
            [targetLabel eid [EnemyAttack pid eid, ChooseEnemyAttack pid (filter (/= eid) enemies)] | eid <- enemies]
    pure g
  EnemyAttack pid eid -> do
    many <- elem MayDeclareMultipleDefenders <$> getModifiers (PlayerTarget pid)
    defenders <- readyCharacters pid
    enemyName <- cardVar "enemy" <$> getEnemy eid
    let declare =
          if many
            then DeclareDefenders pid eid []
            else
              questionLabelWith (ikeyWith "label.declareDefender" enemyName) (Just $ toTarget eid) [] pid
                $ [targetLabel d [Exhaust d, DefendWith pid eid d] | d <- defenders]
                <> [Label (ikey "label.takeAttackUndefended") [UndefendedAttack pid eid]]
    pushAll =<< inLogBlock (EnemyAttackBlock pid eid) [CheckWindows [mkWhen $ EnemyAttacking eid pid], declare]
    pure g
  DefendWith pid eid defender -> do
    pushAll
      [ CheckWindows [mkAfter $ DeclaredAsDefender defender eid]
      , ResolveShadow eid pid (Just defender)
      , AttackDamage eid defender
      , CheckWindows [mkAfter $ EnemyAttacking eid pid, mkAfter $ DefendedAttack defender eid]
      ]
    pure g
  DeclareDefenders pid eid chosen -> do
    defenders <- readyCharacters pid
    enemyName <- cardVar "enemy" <$> getEnemy eid
    push
      $ questionLabelWith (ikeyWith "label.declareDefenders" enemyName) (Just $ toTarget eid) chosen pid
      $ [ targetLabel d [Exhaust d, CheckWindows [mkAfter $ DeclaredAsDefender d eid], DeclareDefenders pid eid (chosen <> [d])]
        | d <- defenders
        ]
      <> case chosen of
        [] -> [Label (ikey "label.takeAttackUndefended") [UndefendedAttack pid eid]]
        first : _ ->
          [ Label (ikey "label.doneDeclaringDefenders")
              [ ResolveShadow eid pid (Just first)
              , DefendedByMany pid eid chosen
              , CheckWindows $ mkAfter (EnemyAttacking eid pid) : [mkAfter $ DefendedAttack d eid | d <- chosen]
              ]
          ]
    pure g
  DefendedByMany pid eid (filter (inPlay g) -> []) -> g <$ push (UndefendedDamage pid eid)
  DefendedByMany pid eid (filter (inPlay g) -> defenders) -> do
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
      $ questionLabelWith (ikey "label.undefendedDamage") (Just $ toTarget eid) [] pid choices
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
      $ questionLabelWith (ikey "label.attackStagingAlone") (Just attacker) [] pid
      $ [ targetLabel eid [AttackWith attacker, CreateEffect $ Effect GameSource attacker [AttackModifier 1] UntilEndOfPhase, ResolveAttack pid eid]
        | eid <- staged
        ]
      <> [Label (ikey "label.doNotAttack") []]
    pure g
  DeclareAttackers pid eid -> do
    attackers <- readyCharacters pid
    push
      $ if null attackers
        then ResolveAttack pid eid
        else
          questionLabelWith (ikey "label.declareAttackers") Nothing [toTarget eid] pid
            $ [targetLabel a [AttackWith a, DeclareAttackers pid eid] | a <- attackers]
            <> [Label (ikey "label.doneAttacking") [ResolveAttack pid eid]]
    pure g
  ResolveAttack pid eid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> AttackingHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> AttackingAlly)
    let attackers = map toTarget heroes <> map toTarget allies
    case Map.lookup eid g.entities.enemies of
      -- the enemy left play before the attack resolved: the attackers just stop
      Nothing -> pushAll (map StopAttacking attackers)
      Just enemy' -> do
        attack <- getAttackStrength attackers eid (toCardDef enemy')
        defense <- getEnemyDefense eid
        if null attackers
          then pure ()
          else
            pushAll
              =<< inLogBlock
                (PlayerAttackBlock pid eid attackers)
                (map StopAttacking attackers <> [DealDamage (PlayerSource pid) (toTarget eid) (attack - defense), AttackResolved eid attackers])
    pure g
  AttackResolved eid attackers -> do
    destroyed <- not <$> selectAny (EnemyWithId eid)
    pushWhen destroyed $ CheckWindows [mkAfter $ EnemyDestroyedByAttack eid attackers]
    pure g
  -- a defender that left play before the damage leaves the attack undefended
  AttackDamage eid defender | not (inPlay g defender) -> do
    lead <- getLead
    engaged <- placementController . (.placement) <$> getEnemy eid
    push $ UndefendedDamage (fromMaybe lead engaged) eid
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
      -- each shadow card turns face up, and one with an effect resolves it
      pushAll
        $ concat
          [ RevealShadowCard eid (ecId card)
              : if cdShadowEffect (toCardDef card)
                then [CheckWindows [mkWhen $ ShadowEffectTriggered (ecId card) eid], ResolveShadowCard (ecId card) eid pid mDefender]
                else []
          | card <- enemy.shadowCards
          ]
    pure g
  RevealShadowCard eid cid -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      when (any ((== cid) . ecId) enemy.shadowCards) $ send $ FlipCard cid
    pure g
  ResolveShadowCard cid eid pid mDefender -> do
    for_ (Map.lookup eid g.entities.enemies) \enemy ->
      for_ (find ((== cid) . ecId) enemy.shadowCards) \card -> do
        push $ CheckWindows [mkAfter $ ShadowEffectResolved cid eid]
        resolveShadowEffect card (ResolveShadowEffect cid eid pid mDefender)
    pure g
  ShuffleIntoEncounterDeck (ObjectiveTarget oid) | Just o <- Map.lookup oid g.entities.objectives -> do
    deck <- shuffleM (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o) : (toAttrs g.scenario).encounterDeck)
    pure $ g & entitiesL . objectivesL %~ Map.delete oid & scenarioL %~ overAttrs (encounterDeckL .~ deck)
  PutShadowTreacheryIntoPlay host cid placement -> do
    for_ (Map.lookup host g.entities.enemies) \hostEnemy ->
      for_ (find ((== cid) . ecId) hostEnemy.shadowCards) \card -> do
        thisTreachery <- overAttrs (\a -> a {treacheryPlacement = placement}) <$> newTreachery card
        pushAll [RemoveShadowCard host cid]
        putGame $ g & entitiesL . treacheriesL %~ insertEntity thisTreachery
    getGame
  ChooseAndExhaust source pid n | n > 0 -> do
    ready <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
    unless (null ready)
      $ push
      $ questionLabelWith (ikey "label.chooseAndExhaustCharacter") (sourceTarget source) [] pid
        [targetLabel c [Exhaust c, ChooseAndExhaust source pid (n - 1)] | c <- ready]
    pure g
  ChooseAndExhaustHero source pid -> do
    ready <- map toTarget <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
    unless (null ready)
      $ push
      $ questionLabelWith (ikey "label.chooseAndExhaustHero") (sourceTarget source) [] pid [targetLabel h [Exhaust h] | h <- ready]
    pure g
  ChooseAndDiscard source pid targets -> do
    unless (null targets)
      $ push
      $ questionLabelWith (ikey "label.chooseCardToDiscard") (sourceTarget source) [] pid [targetLabel t [Discard t] | t <- targets]
    pure g
  BeginPhase phase -> pure $ g & phaseL .~ phase
  CommitCharacters pid -> do
    heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> not_ CommittedHero)
    allies <- select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> not_ CommittedAlly)
    barred <- elem CannotCommitCharacters <$> getModifiers (PlayerTarget pid)
    let characters = if barred then [] else map toTarget heroes <> map toTarget allies
    quests <- getsGame (map toTarget . toList . entitiesQuests . gameEntities)
    -- a character's action may be worth more than its willpower, as Faramir's, so the
    -- player's actions are offered alongside committing it
    actions <- getActionAbilities pid
    -- asked even with no one left to commit, so the player moves on to staging
    push
      $ questionLabelWith (ikey "label.commitCharacters") Nothing quests pid
      $ [ targetLabel c [CommitToQuest c, CheckWindows [mkAfter $ CommittedToQuest c], CommitCharacters pid]
        | c <- characters
        ]
      <> [AbilityLabel pid ab [] [CommitCharacters pid] | ab <- actions]
      <> [Label (ikey "label.doneCommitting") []]
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
      | willpower > threat -> pushAll [PlaceProgressOnQuest (willpower - threat), CheckWindows [mkAfter QuestedSuccessfully]]
      | threat > willpower -> pushAll $ [RaiseThreat pid (threat - willpower) | pid <- players] <> [CheckWindows [mkAfter QuestedUnsuccessfully]]
      | otherwise -> pure ()
    pure $ g & lastCommittedWillpowerL ?~ willpower
  PlaceTokens _ target@(QuestTarget _) Progress n | n > 0 -> do
    push $ CheckWindows [mkAfter $ ProgressPlaced target n]
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
  PayForCard pid card windows -> do
    costId <- getRandom
    extra <- costIncrease card
    push $ PayCost costId
    let activeCost =
          ActiveCost
            { activeCostId = costId
            , activeCostCost = raiseCost extra (cardCost card)
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
    thisAlly <- newAlly pid card
    limited' <- elem OneAllyPerRound <$> getModifiers GameTarget
    pushWhen limited' $ CreateEffect $ Effect GameSource GameTarget [AllyPlayedThisRound] UntilEndOfRound
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutAllyIntoPlayUnder pid card -> do
    thisAlly <- newAlly (fromMaybe pid card.owner) card
    push $ CheckWindows [mkAfter $ EntersPlay $ toTarget thisAlly]
    pure $ g & entitiesL . alliesL %~ insertEntity (overAttrs (\a -> a {allyPlacement = InPlayArea pid}) thisAlly)
  PutHeroIntoPlay pid card -> do
    thisHero <- newHero pid card
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
          playerWindow (ikey "label.useResponse") msg pid
            $ [AbilityLabel pid ab windows [ResponseWindow windows order (abilityKey ab : used) 0] | ab <- available]
            <> fromHand
            <> [Label (ikey "label.pass") [next]]
    pure g
  UseAbility pid ab windows -> do
    pushAll
      =<< inLogBlock
        (AbilityBlock pid ab.abilitySource (isForcedAbility ab))
        (costMessages (PlayerSource pid) (abilityCost ab) <> [UseCardAbility pid ab.abilitySource ab.abilityIndex windows])
    pure g
  PlayEvent pid card _ -> do
    for_ (lookupEvent (toCardCode card) card.id pid) $ void . runEach msg
    pure g
  ReturnToHand target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    leftPlay g target
    pushAll [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
    pure $ g & entitiesL . alliesL %~ Map.delete aid & playersL . ix a.owner . handL %~ (<> [toCard a])
  ShuffleIntoOwnersDeck target@(AllyTarget aid) | Just a <- Map.lookup aid g.entities.allies -> do
    leftPlay g target
    pushAll [Discard (toTarget att) | att <- toList g.entities.attachments, att.placement == AttachedTo target]
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
      ObjectiveTarget oid | Just o <- Map.lookup oid g.entities.objectives ->
        pure $ g & entitiesL . objectivesL %~ Map.delete oid & toTop (MkEncounterCard (objectiveCardId $ toAttrs o) (toCardCode o))
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
  EngageOrcGuard pid card -> do
    guard' <- newEnemy (MkEncounterCard card.id (toCardCode orcGuard))
    push $ CheckWindows [mkAfter $ EnemyEngaged guard'.id pid]
    pure
      $ g
      & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = EngagedWith pid}) guard')
      & scenarioL %~ overAttrs (orcGuardsL %~ Map.insert card.id card)
  PutObjectiveIntoPlay card -> do
    thisObjective <- newObjective card
    push $ GuardObjective (toId thisObjective)
    pure $ g & entitiesL . objectivesL %~ insertEntity (overAttrs (\a -> a {objectivePlacement = StagingArea}) thisObjective)
  TakeControl aid pid -> pure $ g & entitiesL . alliesL . ix aid %~ overAttrs (\a -> a {allyPlacement = InPlayArea pid})
  CompleteQuest qid -> do
    push AdvanceQuest
    pure $ g & entitiesL . questsL %~ Map.delete qid
  ResetToStage card _ -> do
    pushAll [PutQuestIntoPlay card, Flip (toTarget card)]
    pure $ g & entitiesL . questsL .~ mempty
  PlayAttachment pid card -> do
    attachment <- newAttachment pid card
    hosts <- attachmentHosts (toAttrs attachment).attachTo
    let attachTo host = targetLabel host [AttachCard (toId attachment) host, CheckWindows [mkAfter $ EntersPlay $ toTarget attachment]]
    push $ case hosts of
      [host] -> uiToRun (attachTo host)
      _ -> questionLabelWith (ikey "label.attachTo") (Just $ toTarget attachment) hosts pid (map attachTo hosts)
    pure $ g & entitiesL . attachmentsL %~ insertEntity attachment
  PutQuestIntoPlay card -> do
    thisQuest <- newQuest card
    pure $ g & entitiesL . questsL %~ insertEntity thisQuest
  RevealCard card -> do
    let def = toCardDef card
    players <- getPlayerOrder
    let surge = [RevealEncounterCard | Surge `member` cdKeywords def]
        doomed = [RaiseThreat p n | Doomed n <- toList (cdKeywords def), p <- players]
        whenRevealed t = if cdWhenRevealed def then AskToResolveWhenRevealed t card else WhenRevealed t
        revealing t = inLogBlock (RevealBlock card) $ [CheckWindows [mkWhen $ Revealed t], whenRevealed t] <> doomed <> [CheckWindows [mkAfter $ Revealed t]]
    case cdCardType def of
      EnemyType -> do
        enemy <- newEnemy card
        pushAll . (<> surge) =<< revealing (toTarget enemy)
        pure $ g & entitiesL . enemiesL %~ insertEntity (overAttrs (\a -> a {enemyPlacement = StagingArea}) enemy)
      LocationType -> do
        location <- newLocation card
        pushAll . (<> surge) =<< revealing (toTarget location)
        pure $ g & entitiesL . locationsL %~ insertEntity (overAttrs (\a -> a {locationPlacement = StagingArea}) location)
      TreacheryType -> do
        treachery <- newTreachery card
        pushAll . (<> (FinishRevelation (toId treachery) : surge)) =<< revealing (toTarget treachery)
        pure $ g & entitiesL . treacheriesL %~ insertEntity treachery
      ObjectiveType -> do
        pushAll =<< inLogBlock (RevealBlock card) [PutObjectiveIntoPlay card]
        pure g
      _ -> do
        pushAll =<< inLogBlock (RevealBlock card) []
        pure $ g & scenarioL %~ overAttrs (encounterDiscardL %~ (card :))
  AskToResolveWhenRevealed t card -> do
    lead <- getLead
    push $ questionLabelWith (ikeyWith "label.resolveWhenRevealed" (cardVar "card" card)) (Just t) [] lead [targetLabel t [WhenRevealed t]]
    pure g
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

-- | Show the players what the board does as it happens: the log is written by the narrator.
announce :: Message -> GameT ()
announce = \case
  -- a stage turning over, as the setup or a click does
  Flip target -> do
    quests <- getsGame (toList . entitiesQuests . gameEntities)
    for_ quests \q -> do
      let attrs = toAttrs q
      when (not attrs.questFlipped && (target == toTarget q || target == CardIdTarget attrs.questCardId))
        $ send
        $ FlipCard attrs.questCardId
  _ -> pure ()

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
  -- an objective that leaves its hero, or is discarded, goes back to the staging area
  ObjectiveTarget oid -> do
    toTop <- elem LeavesPlayToTopOfEncounterDeck <$> getModifiers target
    g <$ push (if toTop then ReturnToTopOfEncounterDeck target else PlaceObjective oid StagingArea)
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
        <> [Discard (toTarget o) | o <- toList g.entities.objectives, o.placement == AttachedTo target]
      mods <- getModifiers target
      let removed = g & entitiesL . l %~ Map.delete k
          orcGuards = (toAttrs g.scenario).scenarioOrcGuards
      case toCard entity of
        -- an Orc Guard leaves its player card in that player's discard pile
        EncounterCard ec
          | Just pc <- Map.lookup ec.id orcGuards ->
              pure $ addToDiscard (PlayerCard pc) $ removed & scenarioL %~ overAttrs (orcGuardsL %~ Map.delete ec.id)
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
  filterM (fmap (notElem CannotAttackOrDefend) . getModifiers) $ map toTarget heroes <> map toTarget allies

-- | Shadow cards are not in play, so the card's own module resolves the effect
-- through a temporary instance of it.
resolveShadowEffect :: EncounterCard -> Message -> GameT ()
resolveShadowEffect card msg = case cdCardType (toCardDef card) of
  EnemyType -> void . runEach msg . createEnemy card =<< getRandom
  LocationType -> void . runEach msg . createLocation card =<< getRandom
  TreacheryType -> void . runEach msg . createTreachery card =<< getRandom
  _ -> pure ()


-- | The damage an enemy's attack deals to a character, and, for an enemy whose excess
-- damage counts against the player, the threat it raises.
attackDamage :: PlayerId -> EnemyId -> Target -> Int -> GameT [Message]
attackDamage pid eid target n = do
  excessToThreat <- elem ExcessDamageToThreat <$> getModifiers eid
  remaining <- (-) <$> getHitPoints target <*> getDamage target
  let excess = if excessToThreat then max 0 (n - remaining) else 0
  pure $ DealDamage (EnemySource eid) target n : [RaiseThreat pid excess | excess > 0]

-- | The enemy a step of an enemy's attack, or of an attack on one, is about.
attackingEnemy :: Message -> Maybe EnemyId
attackingEnemy = \case
  EnemyAttack _ eid -> Just eid
  DefendWith _ eid _ -> Just eid
  DeclareDefenders _ eid _ -> Just eid
  DefendedByMany _ eid _ -> Just eid
  UndefendedAttack _ eid -> Just eid
  UndefendedDamage _ eid -> Just eid
  AttackDamage eid _ -> Just eid
  ResolveShadow eid _ _ -> Just eid
  DeclareAttackers _ eid -> Just eid
  _ -> Nothing

-- | Whether a character is still in play.
inPlay :: Game -> Target -> Bool
inPlay g = \case
  HeroTarget hid -> Map.member hid g.entities.heroes
  AllyTarget aid -> Map.member aid g.entities.allies
  _ -> True
