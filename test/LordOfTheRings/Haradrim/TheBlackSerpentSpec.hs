module LordOfTheRings.Haradrim.TheBlackSerpentSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (cdCardType, cdKeywords, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (GuardedBy))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (PlayerWithId))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theBlackSerpent)
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (encounterCardOf)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds, RoundEnds), mkWhen)
import TestImport

serpentTest :: (PlayerId -> GameT ()) -> IO ()
serpentTest = gameTestIn theBlackSerpent testDeck

objectives :: GameT [ObjectiveAttrs]
objectives = getsGame (map toAttrs . toList . (.entities.objectives))

objectiveOf :: CardDef -> GameT ObjectiveAttrs
objectiveOf def = fromJustNote "no such objective" . find ((== toCardCode def) . objectiveCardCode) <$> objectives

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

-- | The objective revealed, its Guarded (X) resolved.
reveal :: CardDef -> GameT ObjectiveAttrs
reveal def = do
  run . RevealCard =<< genEncounterCard def
  objectiveOf def

-- | The objective put straight onto the hero, as claimed.
onHero :: CardDef -> Target -> GameT ObjectiveAttrs
onHero def h = do
  card <- genEncounterCard def
  run $ PutObjectiveIntoPlayAttached card h
  objectiveOf def

hero :: GameT Target
hero = toTarget <$> getHeroWithCode "01012"

mainQuest :: CardDef -> Int -> GameT Target
mainQuest def n = do
  q <- putQuestStage def
  run $ Flip q
  when (n > 0) $ run $ PlaceTokens GameSource q Progress n
  pure q

progressOn :: Target -> GameT Int
progressOn t = getsGame (maybe 0 (.progress) . find ((== t) . toTarget) . toList . (.entities.quests))

setAsideCards :: [CardDef] -> GameT ()
setAsideCards defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

endEncounterPhase :: GameT ()
endEncounterPhase = run $ CheckWindows [mkWhen (PhaseEnds EncounterPhase)]

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

spec :: Spec
spec = describe "The Black Serpent" do
  it "is a scenario the game can be dealt" . serpentTest $ \_ ->
    isJust (lookupScenario "the-black-serpent") `shouldBe` True

  -- \* Setup

  it "adds The Black Serpent to the staging area and reveals 1 random Harad objective, guarded by a card of its types" . serpentTest $ \pid -> do
    flipFirstQuest pid
    serpent <- enemyOf Enemies.theBlackSerpent
    getsGame (fmap (.placement) . lookup serpent . (.entities.enemies)) `shouldReturn` Just StagingArea
    [o] <- objectives
    o.placement `shouldBe` StagingArea
    Just guardT <- encounterCardOf o
    let types = [t | GuardedBy ts <- toList (cdKeywords $ toCardDef o), t <- ts]
    guardType <- case guardT of
      EnemyTarget e -> cdCardType . toCardDef <$> getsGame (fromJustNote "guard" . lookup e . (.entities.enemies))
      LocationTarget l -> cdCardType . toCardDef <$> getLocation l
      _ -> error "not an encounter card"
    guardType `shouldSatisfy` (`elem` types)
    length . scenarioSetAside . toAttrs <$> getsGame (.scenario) `shouldReturn` 2
    -- 41 cards: the 3 objectives and The Black Serpent are out of the deck
    deck <- getEncounterDeck
    discard <- getEncounterDiscard
    length deck + length discard + 1 `shouldBe` 37
    getActiveQuest >>= (`shouldSatisfy` (.flipped)) . toAttrs

  it "advances from stage 1 to Battle in the Camp, which reveals another objective, once the first is on a hero" . serpentTest $ \pid -> do
    flipFirstQuest pid
    [o] <- objectives
    Just guardT <- encounterCardOf o
    run $ Discard guardT
    chooseTarget pid =<< hero
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 6
    resolveWhenRevealed pid
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.battleInTheCamp
    length <$> objectives `shouldReturn` 2
    length . scenarioSetAside . toAttrs <$> getsGame (.scenario) `shouldReturn` 1

  -- \* Guarded (X)

  it "discards encounter cards until one of an objective's Guarded (X) types, which guards it from the staging area" . serpentTest $ \_ -> do
    _ <- setEncounterDeck [Treacheries.noQuarter, Enemies.southronArcher, Locations.desertBastion]
    banner <- reveal Objectives.bannerOfTheSerpent
    archer <- enemyOf Enemies.southronArcher
    encounterCardOf banner `shouldReturn` Just (EnemyTarget archer)
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Treacheries.noQuarter]
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.desertBastion]

  it "passes over enemies for Summons from Mordor, guarded by a location" . serpentTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.serpentGuard, Locations.desertBastion]
    summons <- reveal Objectives.summonsFromMordor
    bastion <- locationOf Locations.desertBastion
    encounterCardOf summons `shouldReturn` Just (LocationTarget bastion)

  -- \* The stages

  it "has the first player claim a Harad objective once its guard leaves play, and blocks the stage until then" . serpentTest $ \pid -> do
    q <- mainQuest Quests.theCrossroadsFort 0
    _ <- setEncounterDeck [Enemies.southronArcher]
    _ <- reveal Objectives.theSerpentsGarb
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
    run $ PlaceTokens GameSource q Progress 6
    progressOn q `shouldReturn` 6
    run . Discard . EnemyTarget =<< enemyOf Enemies.southronArcher
    h <- hero
    chooseTarget pid h
    (.placement) <$> objectiveOf Objectives.theSerpentsGarb `shouldReturn` AttachedTo h
    -- with each objective attached, the stage is defeated with the progress it has: with no
    -- stage left, the game is won
    getsGame (.outcome) `shouldReturn` Just Won

  it "has the first player claim at once an objective that found no guard" . serpentTest $ \pid -> do
    _ <- mainQuest Quests.theCrossroadsFort 0
    _ <- setEncounterDeck [Treacheries.noQuarter]
    _ <- reveal Objectives.theSerpentsGarb
    h <- hero
    chooseTarget pid h
    (.placement) <$> objectiveOf Objectives.theSerpentsGarb `shouldReturn` AttachedTo h

  it "reveals a set-aside objective as Battle in the Camp is revealed, and engages The Black Serpent with each player controlling one" . serpentTest $ \pid -> do
    stage Enemies.theBlackSerpent
    serpent <- enemyOf Enemies.theBlackSerpent
    setAsideCards [Objectives.bannerOfTheSerpent]
    _ <- setEncounterDeck [Locations.southronCamp]
    q <- putQuestStage Quests.battleInTheCamp
    run $ Flip q
    resolveWhenRevealed pid
    banner <- objectiveOf Objectives.bannerOfTheSerpent
    isJust <$> encounterCardOf banner `shouldReturn` True
    getModifiers serpent >>= (`shouldSatisfy` notElem (EngagedWithEachPlayer (PlayerWithId pid)))
    run . Discard . LocationTarget =<< locationOf Locations.southronCamp
    chooseTarget pid =<< hero
    getModifiers serpent >>= (`shouldSatisfy` elem (EngagedWithEachPlayer (PlayerWithId pid)))

  it "attaches a set-aside objective to The Black Serpent as Defeat the Serpent is revealed, and wins once it is defeated" . serpentTest $ \pid -> do
    stage Enemies.theBlackSerpent
    serpent <- enemyOf Enemies.theBlackSerpent
    setAsideCards [Objectives.summonsFromMordor]
    _ <- setEncounterDeck [Locations.southronCamp, Locations.desertBastion]
    q <- putQuestStage Quests.defeatTheSerpent
    run $ Flip q
    resolveWhenRevealed pid
    summons <- objectiveOf Objectives.summonsFromMordor
    encounterCardOf summons `shouldReturn` Just (EnemyTarget serpent)
    -- 1 encounter card revealed for the 1 player
    length <$> getEncounterDeck `shouldReturn` 1
    getModifiers serpent >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ PlaceTokens GameSource q Progress 6
    getModifiers serpent >>= (`shouldSatisfy` notElem CannotTakeDamage)
    getsGame (.outcome) `shouldReturn` Nothing
    run $ Destroy GameSource (EnemyTarget serpent)
    chooseTarget pid =<< hero
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* The Black Serpent

  it "gives The Black Serpent +X threat, attack and defense for the stage, and keeps it from damage" . serpentTest $ \_ -> do
    _ <- putQuestStage Quests.battleInTheCamp
    stage Enemies.theBlackSerpent
    serpent <- enemyOf Enemies.theBlackSerpent
    getEnemyThreat serpent `shouldReturn` 2
    getEnemyAttack serpent `shouldReturn` 4
    getEnemyDefense serpent `shouldReturn` 3
    run $ DealDamage GameSource (EnemyTarget serpent) 3
    getDamage (EnemyTarget serpent) `shouldReturn` 0

  it "has The Black Serpent remove X progress from the main quest at the end of the encounter phase, or attack the first player" . serpentTest $ \pid -> do
    q <- mainQuest Quests.theCrossroadsFort 3
    stage Enemies.theBlackSerpent
    endEncounterPhase
    chooseLabelStarting "$label.removeMainQuestProgress" pid
    progressOn q `shouldReturn` 2
    run $ RemoveTokens GameSource q Progress 2
    endEncounterPhase
    -- no progress to remove: it attacks
    getQuestion pid >>= (`shouldSatisfy` isJust)

  -- \* The Harad objectives

  it "gives Harad enemies +1 attack while Banner of the Serpent is attached to an encounter card, and -1 to those engaged with its controller" . serpentTest $ \pid -> do
    _ <- setEncounterDeck [Enemies.serpentGuard]
    banner <- reveal Objectives.bannerOfTheSerpent
    guard' <- enemyOf Enemies.serpentGuard
    getEnemyAttack guard' `shouldReturn` 4
    run $ PlaceObjective banner.objectiveId StagingArea
    run . ClaimObjective banner.objectiveId =<< hero
    getEnemyAttack guard' `shouldReturn` 3
    run $ EngageEnemy pid guard'
    run $ BeginPhase EncounterPhase
    chooseAbilityOf pid (ObjectiveTarget banner.objectiveId)
    getEnemyAttack guard' `shouldReturn` 2

  it "gives The Serpent's Garb's encounter card +2 defense and immunity" . serpentTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.serpentGuard]
    _ <- reveal Objectives.theSerpentsGarb
    guard' <- enemyOf Enemies.serpentGuard
    getEnemyDefense guard' `shouldReturn` 3
    getModifiers guard' >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  it "bars reducing threat while Summons from Mordor is attached to an encounter card, and gives its hero a resource" . serpentTest $ \pid -> do
    _ <- setEncounterDeck [Locations.desertBastion]
    _ <- reveal Objectives.summonsFromMordor
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)
    run . Discard . LocationTarget =<< locationOf Locations.desertBastion
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem CannotReduceThreat)
    h <- hero
    summons <- objectiveOf Objectives.summonsFromMordor
    run $ PlaceObjective summons.objectiveId StagingArea
    run $ ClaimObjective summons.objectiveId h
    run $ BeginPhase EncounterPhase
    beravor <- getHeroWithCode "01012"
    resources <- getHeroResources (toId beravor)
    chooseAbilityOf pid (ObjectiveTarget summons.objectiveId)
    getHeroResources (toId beravor) `shouldReturn` (resources + 1)

  -- \* The encounter cards

  it "has Serpent Guard remove 2 progress as it engages, or make engaged enemies undamageable this round" . serpentTest $ \pid -> do
    _ <- mainQuest Quests.theCrossroadsFort 0
    stage Enemies.serpentGuard
    guard' <- enemyOf Enemies.serpentGuard
    run $ EngageEnemy pid guard'
    getModifiers guard' >>= (`shouldSatisfy` elem CannotTakeDamage)

  it "has The Serpent's Redoubt exhaust an ally played from hand when there is no progress to remove" . serpentTest $ \pid -> do
    _ <- mainQuest Quests.theCrossroadsFort 0
    stage Locations.theSerpentsRedoubt
    card <- playWith pid "01002" 2 Allies.guardOfTheCitadel
    getsGame (fmap allyExhausted . find ((== card.id) . allyCardId) . map toAttrs . toList . (.entities.allies)) `shouldReturn` Just True

  it "has Roofless Tower cancel an event played when there is no progress to remove" . serpentTest $ \pid -> do
    _ <- mainQuest Quests.theCrossroadsFort 0
    stage Locations.rooflessTower
    _ <- playWith pid "01002" 2 Events.forGondor
    getAttackMods <- getModifiers =<< hero
    getAttackMods `shouldSatisfy` notElem (AttackModifier 1)

  it "gives Desert Bastion +1 threat for each Harad objective on a hero" . serpentTest $ \_ -> do
    stage Locations.desertBastion
    bastion <- locationOf Locations.desertBastion
    getLocationThreat bastion `shouldReturn` 1
    _ <- onHero Objectives.bannerOfTheSerpent =<< hero
    getLocationThreat bastion `shouldReturn` 2

  it "has Southron Camp remove all progress from each location at the end of the round" . serpentTest $ \_ -> do
    stage Locations.southronCamp
    stage Locations.desertBastion
    bastion <- locationOf Locations.desertBastion
    run $ PlaceTokens GameSource (LocationTarget bastion) Progress 2
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getLocation bastion `shouldReturn` 0

  it "keeps the main quest to its printed quest points while Broken Courtyard is in the staging area" . serpentTest $ \_ -> do
    q <- mainQuest Quests.theCrossroadsFort 5
    stage Locations.brokenCourtyard
    getModifiers q >>= (`shouldSatisfy` elem (ProgressLimit 1))

  it "has Bold and Grim deal 1 damage to each hero with an objective attached" . serpentTest $ \_ -> do
    h <- hero
    _ <- onHero Objectives.bannerOfTheSerpent h
    _ <- setEncounterDeck [Treacheries.boldAndGrim, Locations.southronCamp]
    run RevealEncounterCard
    resolveRevealed
    getDamage h `shouldReturn` 1

  it "has No Quarter detach a Harad objective from a hero and reveal it, guarded anew" . serpentTest $ \pid -> do
    h <- hero
    _ <- onHero Objectives.theSerpentsGarb h
    _ <- setEncounterDeck [Treacheries.noQuarter, Locations.southronCamp, Enemies.serpentGuard, Enemies.southronArcher]
    run RevealEncounterCard
    resolveRevealed
    garb <- objectiveOf Objectives.theSerpentsGarb
    chooseTarget pid (ObjectiveTarget garb.objectiveId)
    guard' <- enemyOf Enemies.serpentGuard
    (encounterCardOf =<< objectiveOf Objectives.theSerpentsGarb) `shouldReturn` Just (EnemyTarget guard')

  it "has the first player choose for Chaos in the Night between threat and no progress on quest cards" . serpentTest $ \pid -> do
    q <- mainQuest Quests.theCrossroadsFort 0
    _ <- setEncounterDeck [Treacheries.chaosInTheNight]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.chaosInTheNight.noProgress" pid
    run $ PlaceTokens GameSource q Progress 2
    progressOn q `shouldReturn` 0
    -- a quest card entering play after it resolves takes none either
    stage Quests.dontLetThemEscape
    side <- getsGame (toTarget . fromJustNote "side quest" . find ((== toCardCode Quests.dontLetThemEscape) . toCardCode) . toList . (.entities.quests))
    run $ PlaceTokens GameSource side Progress 2
    progressOn side `shouldReturn` 0

  it "has Don't Let Them Escape! discard the main quest's progress at the end of the round" . serpentTest $ \_ -> do
    q <- mainQuest Quests.theCrossroadsFort 4
    stage Quests.dontLetThemEscape
    run $ CheckWindows [mkWhen RoundEnds]
    progressOn q `shouldReturn` 0

  it "has Don't Let Them Escape! reveal an encounter card as it is chosen as the current quest" . serpentTest $ \_ -> do
    _ <- mainQuest Quests.theCrossroadsFort 0
    stage Quests.dontLetThemEscape
    side <- getsGame (fromJustNote "side quest" . find ((== toCardCode Quests.dontLetThemEscape) . toCardCode) . toList . (.entities.quests))
    _ <- setEncounterDeck [Locations.southronCamp]
    run $ SetCurrentQuest (toTarget side)
    length <$> getEncounterDeck `shouldReturn` 0
