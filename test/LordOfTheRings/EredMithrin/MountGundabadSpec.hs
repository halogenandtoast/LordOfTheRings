module LordOfTheRings.EredMithrin.MountGundabadSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Entities (enemiesL, questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (AnyCard), EnemyMatcher (..), HeroMatcher (HeroControlledBy), PlayerMatcher (..), getEnemy, select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, mountGundabad)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), cavesDeckL, questDeckL, setAsideL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, PhaseEnds, StagingStepBegins), mkWhen)
import TestImport

gundabadTest :: (PlayerId -> GameT ()) -> IO ()
gundabadTest = gameTestIn mountGundabad testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup, Wormsbane attached to Beravor.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  chooseTarget pid =<< hero "01012"

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

activeCodes :: GameT [CardCode]
activeCodes = getsGame (map toCardCode . filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations))

-- | A stage put into play on its B side.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

setQuestDeck :: [CardDef] -> GameT [EncounterCard]
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
  pure cards

-- | N quest cards in the victory display.
questsInVictory :: Int -> GameT ()
questsInVictory n = do
  cards <- replicateM n (genEncounterCard Quests.theUpperLevels)
  g <- (scenarioL %~ overAttrs (victoryDisplayL .~ map EncounterCard cards)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

activeQuestFlipped :: GameT Bool
activeQuestFlipped = (.flipped) . toAttrs <$> getActiveQuest

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

wormsbaneOn :: GameT [(CardCode, Placement)]
wormsbaneOn = getsGame (map (\o -> (toCardCode o, (toAttrs o).placement)) . toList . (.entities.objectives))

setOf :: EncounterCard -> Maybe EncounterSet
setOf = cdEncounterSet . toCardDef

spec :: Spec
spec = describe "Mount Gundabad" do
  it "is a scenario the game can be dealt" . gundabadTest $ \_ ->
    isJust (lookupScenario "mount-gundabad") `shouldBe` True

  -- \* Setup

  it "stages Dagnir, sets The First Forge aside, builds the Caves and quest decks, and attaches Wormsbane Broken side up" . gundabadTest $ \pid -> do
    setUp pid
    enemyCodes `shouldReturn` [toCardCode Enemies.dagnirMountGundabad]
    attrs <- scenarioAttrs
    map toCardCode attrs.scenarioSetAside `shouldBe` [toCardCode Locations.theFirstForge]
    -- Lost Caves, and Throat of the Mountain and Dagnir's Hoard by Caleb's ruling
    length attrs.cavesDeck `shouldBe` 8
    all (\c -> setOf c == Just LostCaves || toCardCode c `elem` map toCardCode [Locations.throatOfTheMountain, Locations.dagnirsHoard]) attrs.cavesDeck
      `shouldBe` True
    any ((`elem` map toCardCode [Locations.throatOfTheMountain, Locations.dagnirsHoard, Objectives.wormsbaneReforged]) . toCardCode) attrs.encounterDeck
      `shouldBe` False
    sort (map toCardCode attrs.questDeck)
      `shouldBe` sort
        (map toCardCode [Quests.theUpperLevels, Quests.theLowerDeeps, Quests.frenziedWarrens, Quests.dagnirsWrath, Quests.forebodingDarkness, Quests.imminentPursuit, Quests.theGreatWorm])
    beravor <- hero "01012"
    wormsbaneOn `shouldReturn` [(toCardCode Objectives.wormsbane, AttachedTo beravor)]
    getModifiers beravor >>= (`shouldSatisfy` elem (AttackModifier 1))
    activeQuestCode `shouldReturn` toCardCode Quests.theSideDoor
    activeQuestFlipped `shouldReturn` True

  -- \* The Side Door

  it "puts 1 of the top 2 stages on the bottom at the end of the planning phase, goes to the victory display and reveals the next stage side 2A" . gundabadTest $ \pid -> do
    setUp pid
    dagnir <- enemyOf Enemies.dagnirMountGundabad
    run $ EngageEnemy pid dagnir
    [a, b] <- take 2 . (.questDeck) <$> scenarioAttrs
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    chooseTarget pid (CardIdTarget a.id)
    victoryCodes `shouldReturn` [toCardCode Quests.theSideDoor]
    activeQuestCode `shouldReturn` toCardCode b
    activeQuestFlipped `shouldReturn` False
    map (.id) . lastMay . (.questDeck) <$> scenarioAttrs `shouldReturn` Just a.id
    -- When Revealed: Return Dagnir to the staging area.
    (.placement) <$> getEnemy dagnir `shouldReturn` StagingArea

  -- \* Exploring Gundabad

  it "flips a stage 2A at the beginning of the quest phase, not before" . gundabadTest $ \_ -> do
    _ <- putQuestStage Quests.theUpperLevels
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    activeQuestFlipped `shouldReturn` False
    run $ CheckWindows [mkWhen $ PhaseBegins QuestPhase]
    activeQuestFlipped `shouldReturn` True

  it "takes no progress on side 2A" . gundabadTest $ \_ -> do
    q <- putQuestStage Quests.frenziedWarrens
    run $ PlaceTokens GameSource q Progress 5
    (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 0

  it "bypasses a stage at the end of the combat phase: to the bottom of the quest deck, its progress gone, the next stage 2A revealed" . gundabadTest $ \pid -> do
    [next] <- setQuestDeck [Quests.theLowerDeeps]
    q <- stageB Quests.frenziedWarrens
    run $ PlaceTokens GameSource q Progress 2
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    chooseAbilityOf pid q
    activeQuestCode `shouldReturn` toCardCode next
    activeQuestFlipped `shouldReturn` False
    map toCardCode . (.questDeck) <$> scenarioAttrs `shouldReturn` [toCardCode Quests.frenziedWarrens]
    victoryCodes `shouldReturn` []

  it "adds a defeated stage to the victory display and reveals the next" . gundabadTest $ \_ -> do
    _ <- setQuestDeck [Quests.theLowerDeeps]
    _ <- stage Enemies.dagnirMountGundabad
    q <- stageB Quests.frenziedWarrens
    run $ PlaceTokens GameSource q Progress 5
    victoryCodes `shouldReturn` [toCardCode Quests.frenziedWarrens]
    activeQuestCode `shouldReturn` toCardCode Quests.theLowerDeeps

  it "raises each player's threat by 1 after commitment on The Upper Levels, and shuffles The First Forge into the Caves deck as it is defeated" . gundabadTest $ \pid -> do
    forge <- genEncounterCard Locations.theFirstForge
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [forge])
    _ <- setQuestDeck [Quests.theLowerDeeps]
    q <- stageB Quests.theUpperLevels
    threat0 <- threatOf pid
    run $ CheckWindows [mkWhen StagingStepBegins]
    threatOf pid `shouldReturn` threat0 + 1
    run $ PlaceTokens GameSource q Progress 3
    map toCardCode . (.cavesDeck) <$> scenarioAttrs `shouldReturn` [toCardCode Locations.theFirstForge]
    (.scenarioSetAside) <$> scenarioAttrs `shouldReturn` []

  it "keeps the first treachery revealed each round from being canceled on The Lower Deeps, and makes a Caves location active as it is defeated" . gundabadTest $ \pid -> do
    _ <- setQuestDeck [Quests.frenziedWarrens]
    q <- stageB Quests.theLowerDeeps
    g <- getGame
    [armory, den] <- traverse genEncounterCard [Locations.lostArmory, Locations.frightfulDen]
    putGame $ g & scenarioL %~ overAttrs (cavesDeckL .~ [armory, den])
    run $ PlaceTokens GameSource q Progress 3
    chooseTarget pid (CardIdTarget armory.id)
    activeCodes `shouldReturn` [toCardCode Locations.lostArmory]
    map (.id) . (.cavesDeck) <$> scenarioAttrs `shouldReturn` [den.id]

  it "adds a random enemy of the top 5 discarded to the staging area on Frenzied Warrens, and reduces each player's threat by 5 as it is defeated" . gundabadTest $ \pid -> do
    _ <- setQuestDeck [Quests.theLowerDeeps]
    q <- stageB Quests.frenziedWarrens
    _ <- setEncounterDeck [Locations.narrowStair, Enemies.guardianOfGundabad, Treacheries.suffocatingShadows, Locations.minesOfGundabad, Locations.desecratedRuins, Enemies.deepDweller]
    run $ RaiseThreat pid 10
    run $ CheckWindows [mkWhen StagingStepBegins]
    enemyCodes `shouldReturn` [toCardCode Enemies.guardianOfGundabad]
    length <$> getEncounterDeck `shouldReturn` 1
    threat0 <- threatOf pid
    run $ PlaceTokens GameSource q Progress 5
    threatOf pid `shouldReturn` threat0 - 5

  it "has Dagnir attack each player with 6 or more characters on Dagnir's Wrath, and deals her 5 damage past her limit as it is defeated" . gundabadTest $ \_ -> do
    _ <- setQuestDeck [Quests.theLowerDeeps]
    dagnir <- stage Enemies.dagnirMountGundabad
    -- 1 more than no quest card: 1 damage a round
    run $ PlaceTokens GameSource (EnemyTarget dagnir) Damage 1
    q <- stageB Quests.dagnirsWrath
    run $ PlaceTokens GameSource q Progress 7
    (.damage) <$> getEnemy dagnir `shouldReturn` 6
    -- X is now 2 (Dagnir's Wrath in the victory display), and her 5 do not count toward it
    run $ PlaceTokens GameSource (EnemyTarget dagnir) Damage 5
    (.damage) <$> getEnemy dagnir `shouldReturn` 7

  it "lets each player play only 1 card each phase on Foreboding Darkness, and flips the top stage over as it is defeated" . gundabadTest $ \pid -> do
    [a, b] <- setQuestDeck [Quests.theLowerDeeps, Quests.frenziedWarrens]
    q <- stageB Quests.forebodingDarkness
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [PlayedACardThisPhase] UntilEndOfPhase
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))
    run $ PlaceTokens GameSource q Progress 4
    chooseTarget pid (CardIdTarget a.id)
    activeQuestCode `shouldReturn` toCardCode b
    activeQuestFlipped `shouldReturn` True

  it "deals 2 damage to a questing character of each player on Imminent Pursuit, and discards a non-unique enemy as it is defeated" . gundabadTest $ \_ -> do
    _ <- setQuestDeck [Quests.theLowerDeeps]
    q <- stageB Quests.imminentPursuit
    beravor <- hero "01012"
    run $ CommitToQuest beravor
    run $ CheckWindows [mkWhen StagingStepBegins]
    getDamage beravor `shouldReturn` 2
    _ <- stage Enemies.dagnirMountGundabad
    _ <- stage Enemies.deepDweller
    run $ PlaceTokens GameSource q Progress 6
    enemyCodes `shouldReturn` [toCardCode Enemies.dagnirMountGundabad]

  it "lowers Dagnir's engagement cost by 5 for each character of the first player on The Great Worm, which must be bypassed with 4 or fewer quests in the victory display" . gundabadTest $ \_ -> do
    _ <- setQuestDeck [Quests.theLowerDeeps]
    dagnir <- stage Enemies.dagnirMountGundabad
    _ <- stageB Quests.theGreatWorm
    getEngagementCost dagnir `shouldReturn` 35
    run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
    activeQuestCode `shouldReturn` toCardCode Quests.theLowerDeeps
    map toCardCode . (.questDeck) <$> scenarioAttrs `shouldReturn` [toCardCode Quests.theGreatWorm]

  it "wins the game as the player with Reforged Wormsbane attacks Dagnir with no hit points left on The Great Worm" . gundabadTest $ \pid -> do
    questsInVictory 5
    dagnir <- stage Enemies.dagnirMountGundabad
    _ <- stageB Quests.theGreatWorm
    beravor <- hero "01012"
    sword <- genEncounterCard Objectives.wormsbaneReforged
    run $ PutObjectiveIntoPlayAttached sword beravor
    -- her 20 hit points taken over earlier rounds
    g <- getGame
    putGame $ g & entitiesL . enemiesL . ix dagnir %~ overAttrs (\a -> a {enemyTokens = addTokens Damage 20 a.enemyTokens})
    inPhase CombatPhase
    run $ AttackWith beravor
    run $ ResolveAttack pid dagnir
    chooseAbilityOf pid =<< getActiveQuest
    getsGame (.outcome) `shouldReturn` Just Won
    -- though with 5 quest cards in the victory display she cannot leave the staging area
    enemyCodes `shouldReturn` []
    victoryCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.dagnirMountGundabad))

  it "offers The Great Worm's response only to the player whose attack it was, with Reforged Wormsbane among its attackers" . twoPlayerTestIn mountGundabad testDeck $ \p1 p2 -> do
    questsInVictory 5
    dagnir <- stage Enemies.dagnirMountGundabad
    _ <- stageB Quests.theGreatWorm
    [wielder, other, _] <- select (HeroControlledBy $ PlayerWithId p1)
    attacker : _ <- select (HeroControlledBy $ PlayerWithId p2)
    sword <- genEncounterCard Objectives.wormsbaneReforged
    run $ PutObjectiveIntoPlayAttached sword (HeroTarget wielder)
    g <- getGame
    putGame $ g & entitiesL . enemiesL . ix dagnir %~ overAttrs (\a -> a {enemyTokens = addTokens Damage 20 a.enemyTokens})
    inPhase CombatPhase
    -- another player's attack
    run $ AttackWith (HeroTarget attacker)
    run $ ResolveAttack p2 dagnir
    getQuestion p1 `shouldReturn` Nothing
    getQuestion p2 `shouldReturn` Nothing
    -- his own attack, without the hero that wields Wormsbane
    run $ AttackWith (HeroTarget other)
    run $ ResolveAttack p1 dagnir
    getQuestion p1 `shouldReturn` Nothing
    getsGame (.outcome) `shouldReturn` Nothing

  -- \* Dagnir

  it "has Dagnir's threat and damage limit be 1 more than the quest cards in the victory display" . gundabadTest $ \_ -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    questsInVictory 2
    getEnemyThreat dagnir `shouldReturn` 3
    run $ PlaceTokens GameSource (EnemyTarget dagnir) Damage 5
    (.damage) <$> getEnemy dagnir `shouldReturn` 3

  it "lets Dagnir take more damage the same round as X rises" . gundabadTest $ \_ -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    run $ PlaceTokens GameSource (EnemyTarget dagnir) Damage 5
    (.damage) <$> getEnemy dagnir `shouldReturn` 1
    questsInVictory 2
    run $ PlaceTokens GameSource (EnemyTarget dagnir) Damage 5
    (.damage) <$> getEnemy dagnir `shouldReturn` 3

  it "leaves Dagnir in play with as much damage as her hit points, as she is Indestructible" . gundabadTest $ \_ -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    g <- getGame
    putGame $ g & entitiesL . enemiesL . ix dagnir %~ overAttrs (\a -> a {enemyTokens = addTokens Damage 19 a.enemyTokens})
    run $ DealDamage GameSource (EnemyTarget dagnir) 1
    (.damage) <$> getEnemy dagnir `shouldReturn` 20
    enemyCodes `shouldReturn` [toCardCode Enemies.dagnirMountGundabad]

  it "keeps Dagnir in the staging area, engaged with each player, with 5 quest cards in the victory display" . gundabadTest $ \pid -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    questsInVictory 5
    run $ EngageEnemy pid dagnir
    (.placement) <$> getEnemy dagnir `shouldReturn` StagingArea
    select (EnemyEngagedWith $ PlayerWithId pid) `shouldReturn` [dagnir]

  it "keeps all but quest damage off Dagnir while Dagnir's Slave is in play" . gundabadTest $ \_ -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    _ <- stage Enemies.dagnirsSlave
    run $ DealDamage GameSource (EnemyTarget dagnir) 1
    (.damage) <$> getEnemy dagnir `shouldReturn` 0
    q <- putQuestStage Quests.theUpperLevels
    run $ DealDamage (QuestSource $ questIdOf q) (EnemyTarget dagnir) 1
    (.damage) <$> getEnemy dagnir `shouldReturn` 1

  -- \* Encounter cards

  it "gives Guardian of Gundabad +2 defense and Desecrated Ruins +2 threat with 3 quest cards in the victory display" . gundabadTest $ \_ -> do
    guardian <- stage Enemies.guardianOfGundabad
    ruins <- stageLocation Locations.desecratedRuins
    getLocationThreat ruins `shouldReturn` 2
    questsInVictory 3
    getModifiers (EnemyTarget guardian) >>= (`shouldSatisfy` elem (DefenseModifier 2))
    getLocationThreat ruins `shouldReturn` 4

  it "returns Deep-dweller to the staging area after it takes damage and survives" . gundabadTest $ \pid -> do
    dweller <- stage Enemies.deepDweller
    run $ EngageEnemy pid dweller
    -- its immediate attack as it engages
    passActions pid
    getQuestion pid >>= \case
      Just _ -> chooseTarget pid =<< hero "01012"
      Nothing -> pure ()
    passActions pid
    run $ DealDamage GameSource (EnemyTarget dweller) 1
    (.placement) <$> getEnemy dweller `shouldReturn` StagingArea

  it "lowers Dagnir's engagement cost by 10 with Mines of Gundabad, and has the players travel to Narrow Stair" . gundabadTest $ \_ -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    _ <- stageLocation Locations.minesOfGundabad
    getEngagementCost dagnir `shouldReturn` 40
    stair <- stageLocation Locations.narrowStair
    getModifiers (LocationTarget stair) >>= (`shouldSatisfy` elem MustBeTraveledTo)

  it "lowers Dagnir's engagement cost by 30 for Wilderland's Bane when she is engaged with no one" . gundabadTest $ \_ -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    reveal [Treacheries.wilderlandsBane]
    getEngagementCost dagnir `shouldReturn` 20

  it "has each player assign X damage among his allies for Suffocating Shadows" . gundabadTest $ \pid -> do
    questsInVictory 1
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Treacheries.suffocatingShadows]
    replicateM_ 2 $ chooseTarget pid ally
    -- 2 damage destroys the Guard of the Citadel
    getsGame (null . (.entities.allies)) `shouldReturn` True

  it "exhausts the hero with Wormsbane as The First Forge becomes active, and reforges Wormsbane onto any hero as it is explored" . gundabadTest $ \pid -> do
    setUp pid
    beravor <- hero "01012"
    forge <- stageLocation Locations.theFirstForge
    run $ PlaceLocation forge ActiveLocation
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    getModifiers beravor >>= (`shouldSatisfy` elem CannotReady)
    questsInVictory 2
    eowyn <- hero "01007"
    run $ PlaceTokens GameSource (LocationTarget forge) Progress 1
    chooseAbilityOf pid (LocationTarget forge)
    chooseTarget pid eowyn
    wormsbaneOn `shouldReturn` [(toCardCode Objectives.wormsbaneReforged, AttachedTo eowyn)]
    getModifiers eowyn >>= (`shouldSatisfy` elem (AttackModifier 3))

  it "returns Dagnir to make an immediate attack against each player as Dagnir's Hoard is explored" . gundabadTest $ \pid -> do
    dagnir <- stage Enemies.dagnirMountGundabad
    run $ EngageEnemy pid dagnir
    hoard <- stageLocation Locations.dagnirsHoard
    run $ PlaceLocation hoard ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget hoard) Progress 7
    (.placement) <$> getEnemy dagnir `shouldReturn` StagingArea
    getQuestion pid >>= (`shouldSatisfy` isJust)

  it "makes a stage of the quest deck the main quest, flipped, as Throat of the Mountain is explored, the old one shuffled back" . gundabadTest $ \pid -> do
    [_, wrath] <- setQuestDeck [Quests.theLowerDeeps, Quests.dagnirsWrath]
    _ <- stageB Quests.frenziedWarrens
    throat <- stageLocation Locations.throatOfTheMountain
    run $ PlaceLocation throat ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget throat) Progress 4
    chooseAbilityOf pid (LocationTarget throat)
    chooseTarget pid (CardIdTarget wrath.id)
    activeQuestCode `shouldReturn` toCardCode Quests.dagnirsWrath
    activeQuestFlipped `shouldReturn` True
    sort . map toCardCode . (.questDeck) <$> scenarioAttrs `shouldReturn` sort (map toCardCode [Quests.theLowerDeeps, Quests.frenziedWarrens])
    -- Throat of the Mountain's own victory point, but not the stage it replaced
    victoryCodes `shouldReturn` [toCardCode Locations.throatOfTheMountain]

  it "puts Wormsbane discarded from a hero still in play in the encounter discard pile, the game not lost" . gundabadTest $ \pid -> do
    setUp pid
    w <- getsGame (fromJustNote "no Wormsbane" . fmap toId . headMay . toList . (.entities.objectives))
    run $ Discard (ObjectiveTarget w)
    getsGame (.outcome) `shouldReturn` Nothing
    wormsbaneOn `shouldReturn` []
    map toCardCode . scenarioEncounterDiscard <$> scenarioAttrs `shouldReturn` [toCardCode Objectives.wormsbane]

  it "loses the game as the hero with Wormsbane leaves play" . gundabadTest $ \pid -> do
    setUp pid
    beravor <- hero "01012"
    run $ Discard beravor
    getsGame (.outcome) `shouldReturn` Just Lost
 where
  questIdOf = \case
    QuestTarget qid -> qid
    _ -> error "not a quest"
