module LordOfTheRings.Dreadnaught.DreadnaughtSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Card (
  CardType (EnemyType, ObjectiveAllyType),
  cdCardType,
  cdUnique,
  toCardDef,
 )
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Helpers.Window (asPlayer, getActionAbilities, getWindowAbilities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getArchery, getEnemy)
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Modifier (ModifierType (CannotBeAttackedByCharactersOf))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught (theHuntForTheDreadnaught)
import LordOfTheRings.Scenario.Types (
  GameMode (..),
  ScenarioAttrs (scenarioCorsairDeck, scenarioEpic, scenarioSetAside),
  modeL,
  setAsideL,
 )
import LordOfTheRings.Window (WindowType (EnemyAttacking, PhaseEnds, RoundEnds), mkAfter, mkWhen)
import TestImport

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "enemy missing"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.enemies)
    )

bossArchery :: EnemyId -> GameT Int
bossArchery eid = getEnemy eid >>= \e -> getArchery (toTarget eid) (toCardDef e)

threePlayerSetup :: GameMode -> GameT () -> IO ()
threePlayerSetup mode body = do
  seats <-
    traverse
      (fmap (either (error . show) id) . uncurry loadPlayer)
      (("one", coreDeck "leadership") :| [("two", coreDeck "spirit"), ("three", coreDeck "tactics")])
  let scenario = overAttrs (modeL .~ mode) theHuntForTheDreadnaught
  env <- newGameEnv (newTableIn scenario seats) (const $ pure ()) 1
  runGameT env body

spec :: Spec
spec = describe "Dreadnaught" do
  it "takes at most five damage per initial local player each round and flips below half health"
    . gameTest
    $ \_ -> do
      eid <- staged Enemies.dreadnaught
      getEnemyHitPoints eid `shouldReturn` 15
      bossArchery eid `shouldReturn` 1
      run $ PlaceTokens GameSource (toTarget eid) Damage 12
      getDamage (toTarget eid) `shouldReturn` 5
      run $ PlaceTokens GameSource (toTarget eid) Damage 4
      getDamage (toTarget eid) `shouldReturn` 5
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (toTarget eid) Damage 3
      run $ CheckWindows [mkWhen RoundEnds]
      toCardCode <$> getEnemy eid `shouldReturn` toCardCode Enemies.dreadnaughtDamaged
      getDamage (toTarget eid) `shouldReturn` 8
      getEnemyHitPoints eid `shouldReturn` 15
      bossArchery eid `shouldReturn` 0

  it "does not flip at exactly half health" . twoPlayerTest (coreDeck "tactics") $ \_ _ -> do
    eid <- staged Enemies.dreadnaught
    run $ PlaceTokens GameSource (toTarget eid) Damage 10
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource (toTarget eid) Damage 5
    run $ CheckWindows [mkWhen RoundEnds]
    toCardCode <$> getEnemy eid `shouldReturn` toCardCode Enemies.dreadnaught
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource (toTarget eid) Damage 1
    run $ CheckWindows [mkWhen RoundEnds]
    toCardCode <$> getEnemy eid `shouldReturn` toCardCode Enemies.dreadnaughtDamaged

  it "uses the fixed initial player count after elimination" . twoPlayerTest (coreDeck "tactics") $ \one _ -> do
    eid <- staged Enemies.dreadnaught
    run $ EliminatePlayer one
    getEnemyHitPoints eid `shouldReturn` 30
    bossArchery eid `shouldReturn` 2
    run $ PlaceTokens GameSource (toTarget eid) Damage 12
    getDamage (toTarget eid) `shouldReturn` 10

  it "releases different random set-aside captains to each player after flipping"
    . twoPlayerTest (coreDeck "tactics")
    $ \one two -> do
      captains <- traverse genEncounterCard [Enemies.ophidian, Enemies.varjax]
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (setAsideL .~ captains)
      eid <- staged Enemies.dreadnaught
      run $ Flip (toTarget eid)
      enemies <- getsGame (toList . (.entities.enemies))
      sort [e.placement | e <- enemies, toId e /= eid] `shouldBe` sort [EngagedWith one, EngagedWith two]
      length enemies `shouldBe` 3
      getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []

  it "the captains bar their engaged player's characters from attacking either Dreadnaught face"
    . gameTest
    $ \pid -> do
      eid <- staged Enemies.dreadnaught
      captain <- staged Enemies.ophidian
      run $ EngageEnemy pid captain
      getModifiers eid >>= (`shouldSatisfy` elem (CannotBeAttackedByCharactersOf pid))
      run $ Flip (toTarget eid)
      getModifiers eid >>= (`shouldSatisfy` elem (CannotBeAttackedByCharactersOf pid))

  it "shares global health while recording only its local capped contribution" . gameTest $ \_ -> do
    let shared = fromJustNote "shared state" $ newSharedEvent 4
    g <- getGame
    putGame
      $ g
      & scenarioL
      %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) shared [] [] [] mempty})
    eid <- staged Enemies.dreadnaught
    getEnemyHitPoints eid `shouldReturn` 60
    let (damaged, _) = applySharedDelta (SharedDelta UUID.nil (GroupOrdinal 1) DreadnaughtDamage 40) shared
    run $ SyncEpic damaged
    getDamage (toTarget eid) `shouldReturn` 40
    run $ PlaceTokens GameSource (toTarget eid) Damage 12
    getDamage (toTarget eid) `shouldReturn` 45
    Just replica <- getsGame (scenarioEpic . toAttrs . (.scenario))
    sharedCounter DreadnaughtDamage replica.epicShared `shouldBe` 45
    map (.deltaAmount) replica.epicPendingDeltas `shouldBe` [5]

  it "keeps a Massive attack on the second player when its defender has left play" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    let addShip pid def = do
          run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
          getsGame
            ( toTarget
                . fromJustNote "ship missing"
                . find ((== toCardCode def) . toCardCode)
                . toList
                . (.entities.allies)
            )
    firstShip <- addShip one Objectives.narelenya
    secondShip <- addShip two Objectives.dreamChaser
    vanished <- addShip two Objectives.corsairSkirmisherShip
    run $ Discard GameSource vanished
    eid <- staged Enemies.dreadnaught
    pushAll [AttackDamage eid vanished, CheckWindows [mkAfter $ EnemyAttacking eid two]]
    void mainLoop
    chooseTarget two secondShip
    getDamage firstShip `shouldReturn` 0
    getDamage secondShip `shouldReturn` 7

  it "inherits shared damage when its group reaches the Dreadnaught after another group" . gameTest $ \_ -> do
    let shared = fromJustNote "shared state" $ newSharedEvent 4
        (damaged, _) = applySharedDelta (SharedDelta UUID.nil (GroupOrdinal 1) DreadnaughtDamage 40) shared
    g <- getGame
    putGame
      $ g
      & scenarioL
      %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) damaged [] [] [] mempty})
    eid <- staged Enemies.dreadnaught
    getDamage (toTarget eid) `shouldReturn` 40
    Just replica <- getsGame (scenarioEpic . toAttrs . (.scenario))
    replica.epicPendingDeltas `shouldBe` []

  it "caps search progress per round and waits until the quest phase ends to advance" . gameTest $ \_ -> do
    target <- putQuestStage Quests.searchingTheHighSeas
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 8
    (.progress) <$> getActiveQuest `shouldReturn` 5
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource target Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 8
    getsGame (.outcome) `shouldReturn` Nothing
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    getsGame (.outcome) `shouldReturn` Just Won

  it "spends stage two progress per initial player to shuffle a set-aside warship" . twoPlayerTest (coreDeck "tactics") $ \one _ -> do
    target <- putQuestStage Quests.destroyTheDreadnaught
    run $ Flip target
    resolveWhenRevealed one
    warship <- genEncounterCard Objectives.gondorianWarship
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [warship])
    run $ PlaceTokens GameSource target Progress 15
    quest <- getActiveQuest
    quest.progress `shouldBe` 10
    abilities <- getActionAbilities one
    let ability = fromJustNote "warship action" $ find ((== QuestSource (toId quest)) . abilitySource) abilities
    run $ UseAbility one ability []
    chooseOptionMatching "choose this group" one (const True)
    (.progress) <$> getActiveQuest `shouldReturn` 0
    map toCardCode
      <$> getEncounterDeck
      >>= (`shouldSatisfy` elem (toCardCode Objectives.gondorianWarship))

  it "offers the Dreadnaught attack cancellation only to the attacked player" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
    target <- putQuestStage Quests.destroyTheDreadnaught
    run $ Flip target
    resolveWhenRevealed one
    eid <- staged Enemies.dreadnaught
    let windows = [mkWhen $ EnemyAttacking eid two]
        isCancellation ability =
          abilityIndex ability == 2 && abilitySource ability == case target of QuestTarget qid -> QuestSource qid; _ -> GameSource
    asPlayer one (getWindowAbilities windows) >>= (`shouldSatisfy` all (not . isCancellation))
    asPlayer two (getWindowAbilities windows) >>= (`shouldSatisfy` any isCancellation)

  it "uses global search progress and still waits for this group's quest phase to end" . gameTest $ \_ -> do
    let shared = fromJustNote "shared state" $ newSharedEvent 4
        (almost, _) = applySharedDelta (SharedDelta UUID.nil (GroupOrdinal 1) SearchingTheHighSeasProgress 31) shared
    g <- getGame
    putGame
      $ g
      & scenarioL
      %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) shared [] [] [] mempty})
    target <- putQuestStage Quests.searchingTheHighSeas
    run $ Flip target
    run $ SyncEpic almost
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceTokens GameSource target Progress 1
    getsGame (.outcome) `shouldReturn` Nothing
    run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
    getsGame (.outcome) `shouldReturn` Just Won

  it "pays four threat to cancel the Dreadnaught's queued attack" . gameTest $ \pid -> do
    target <- putQuestStage Quests.destroyTheDreadnaught
    run $ Flip target
    resolveWhenRevealed pid
    run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.narelenya
    shipTarget <-
      getsGame (toTarget . fromJustNote "ship missing" . headMay . toList . (.entities.allies))
    eid <- staged Enemies.dreadnaught
    pushAll
      [ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      , AttackDamage eid shipTarget
      , CheckWindows [mkAfter $ EnemyAttacking eid pid]
      ]
    void mainLoop
    chooseAbilityOf pid target
    (.threat) <$> getPlayer pid `shouldReturn` 4
    getDamage shipTarget `shouldReturn` 0

  it "prepares the separate Corsair deck and all ships and unique enemies" . gameTestIn theHuntForTheDreadnaught testDeck $ \_ -> do
    run $ DoStep 1 Setup
    attrs <- getsGame (toAttrs . (.scenario))
    length <$> pure (fromMaybe [] attrs.scenarioCorsairDeck) `shouldReturn` 13
    length
      [ card
      | card <- attrs.scenarioSetAside
      , cdCardType (toCardDef card) == EnemyType
      , cdUnique (toCardDef card)
      ]
      `shouldBe` 7
    length [card | card <- attrs.scenarioSetAside, cdCardType (toCardDef card) == ObjectiveAllyType]
      `shouldBe` 10

  it "sets up a solo fleet before the starting hand, without granting an extra ship" . gameTestIn theHuntForTheDreadnaught testDeck $ \pid -> do
    run Setup
    cards <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    let mist =
          fromJustNote "Silent Mist missing" $ find ((== toCardCode Objectives.silentMist) . toCardCode) cards
    chooseTarget pid mist
    length . (.hand) <$> getPlayer pid `shouldReturn` 8
    chooseLabel "$label.keepHand" pid
    chooseTarget pid =<< getActiveQuest
    length <$> getsGame (toList . (.entities.allies)) `shouldReturn` 1
    length <$> getsGame (toList . (.entities.locations)) `shouldReturn` 2
    getsGame (.outcome) `shouldReturn` Nothing

  it "preserves Golden Wing's setup threat reduction" . gameTestIn theHuntForTheDreadnaught testDeck $ \pid -> do
    run Setup
    cards <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    let wing =
          fromJustNote "Golden Wing missing" $ find ((== toCardCode Objectives.goldenWing) . toCardCode) cards
    chooseTarget pid wing
    chooseTarget pid =<< getHeroWithCode "01012"
    (.threat) <$> getPlayer pid `shouldReturn` 24

  it "easy mode removes the specified ships and starts at Cobas Haven" . gameTestIn (overAttrs (modeL .~ EasyMode) theHuntForTheDreadnaught) testDeck $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    map toCardCode deck `shouldSatisfy` notElem (toCardCode Enemies.blackSailsDreadnaught)
    map toCardCode deck `shouldSatisfy` notElem (toCardCode Enemies.corsairWarshipDreadnaught)
    run $ DoStep 6 Setup
    getsGame (map toCardCode . toList . (.entities.locations))
      `shouldReturn` [toCardCode Locations.cobasHavenDreadnaught]

  it "hard mode starts at three Tolfalas Landings" . gameTestIn (overAttrs (modeL .~ HardMode) theHuntForTheDreadnaught) testDeck $ \_ -> do
    run $ DoStep 1 Setup
    run $ DoStep 6 Setup
    getsGame (map toCardCode . toList . (.entities.locations))
      `shouldReturn` replicate 3 (toCardCode Locations.tolfalasLanding)

  it "hard mode limits all threat reduction to one per round" . gameTestIn (overAttrs (modeL .~ HardMode) theHuntForTheDreadnaught) testDeck $ \pid -> do
    run $ RaiseThreat pid 20
    run $ LowerThreat pid 5
    (.threat) <$> getPlayer pid `shouldReturn` 19
    run $ LowerThreat pid 5
    (.threat) <$> getPlayer pid `shouldReturn` 19
    run $ EndEffects UntilEndOfRound
    run $ LowerThreat pid 5
    (.threat) <$> getPlayer pid `shouldReturn` 18

  it "hard mode penalizes player-card healing and readying but not encounter effects" . gameTestIn (overAttrs (modeL .~ HardMode) theHuntForTheDreadnaught) testDeck $ \pid -> do
    hero <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget hero) Damage 3
    run $ HealDamage (HeroSource $ toId hero) (toTarget hero) 1
    (.threat) <$> getPlayer pid `shouldReturn` 1
    run $ HealDamage GameSource (toTarget hero) 1
    (.threat) <$> getPlayer pid `shouldReturn` 1
    run $ Exhaust (toTarget hero)
    runQueueT $ Lifted.ready hero
    void mainLoop
    (.threat) <$> getPlayer pid `shouldReturn` 2
    run $ Exhaust (toTarget hero)
    run $ Ready (toTarget hero)
    (.threat) <$> getPlayer pid `shouldReturn` 2

  it "eliminates a player when their last ship leaves play after fleet preparation" . gameTestIn theHuntForTheDreadnaught testDeck $ \pid -> do
    run Setup
    cards <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    chooseTarget pid
      $ fromJustNote "Silent Mist missing"
      $ find ((== toCardCode Objectives.silentMist) . toCardCode) cards
    chooseLabel "$label.keepHand" pid
    chooseTarget pid =<< getActiveQuest
    shipTarget <-
      getsGame (toTarget . fromJustNote "ship missing" . headMay . toList . (.entities.allies))
    run $ Destroy GameSource shipTarget
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "standard mode stages three locations for a three-player group" $ threePlayerSetup NormalMode do
    run $ DoStep 1 Setup
    run $ DoStep 6 Setup
    length <$> getsGame (toList . (.entities.locations)) `shouldReturn` 3

  it "hard mode stages three Tolfalas Landings and two Wicked Coves for a three-player group" $ threePlayerSetup HardMode do
    run $ DoStep 1 Setup
    run $ DoStep 6 Setup
    codes <- getsGame (map toCardCode . toList . (.entities.locations))
    sort codes
      `shouldBe` sort
        (replicate 3 (toCardCode Locations.tolfalasLanding) <> replicate 2 (toCardCode Locations.wickedCove))
