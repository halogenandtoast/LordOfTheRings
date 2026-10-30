module LordOfTheRings.DreamChaser.TheThingInTheDepthsSpec (spec) where

import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (GainKeyword, ImmuneToPlayerCardEffects))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theThingInTheDepths)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import LordOfTheRings.Matcher (EnemyMatcher (EnemyEngagedWith, EnemyInStagingArea), PlayerMatcher (PlayerWithId), select)
import LordOfTheRings.Phase (Phase (TravelPhase))
import TestImport

thingTest :: (PlayerId -> GameT ()) -> IO ()
thingTest = gameTestIn theThingInTheDepths testDeck

enemies :: GameT [EnemyAttrs]
enemies = getsGame (map toAttrs . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyAttrs
enemyOf def = fromJustNote "no such enemy" . find ((== toCardCode def) . enemyCardCode) <$> enemies

locationOf :: CardDef -> GameT LocationAttrs
locationOf def = getsGame (fromJustNote "no such location" . find ((== toCardCode def) . locationCardCode) . map toAttrs . toList . (.entities.locations))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

fromSet :: EncounterSet -> [EncounterCard] -> [EncounterCard]
fromSet set = filter ((== Just set) . cdEncounterSet . toCardDef)

setAside :: [CardDef] -> GameT ()
setAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

-- | Make the location active: grappling enemies let go of it as it does.
travelTo :: LocationId -> GameT ()
travelTo lid = run $ PlaceLocation lid ActiveLocation

-- | The end of the round, its window alone.
endOfRound :: GameT ()
endOfRound = run $ CheckWindows [mkWhen RoundEnds]

shouldMatchList' :: [EnemyId] -> [EnemyId] -> GameT ()
shouldMatchList' a b = sort a `shouldBe` sort b

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Defend the Ship! in play on its B side, its When Revealed effect resolved against the
-- encounter deck given.
defending :: PlayerId -> [CardDef] -> GameT Target
defending pid deck = do
  _ <- setEncounterDeck deck
  q <- putQuestStage Quests.defendTheShip
  run $ Flip q
  resolveWhenRevealed pid
  pure q

spec :: Spec
spec = describe "The Thing in the Depths" do
  it "is a scenario the game can be dealt" . thingTest $ \_ ->
    isJust (lookupScenario "the-thing-in-the-depths") `shouldBe` True

  it "builds the deck from The Stormcaller and Corsair Raiders, sets the rest aside, and adds Helm of the Stormcaller and an Umbar Raider" . thingTest $ \pid -> do
    flipFirstQuest pid
    s <- scenarioAttrs
    let sets = nub $ mapMaybe (cdEncounterSet . toCardDef) s.scenarioEncounterDeck
    sort sets `shouldBe` sort [EncounterSet.TheStormcaller, EncounterSet.CorsairRaiders]
    -- the Thing and its 18 Creatures and treacheries, and the Stormcaller Elite's enemy sides
    length (fromSet EncounterSet.TheThingInTheDepths s.scenarioSetAside) `shouldBe` 19
    sort (map toCardCode (fromSet EncounterSet.TheStormcallerElite s.scenarioSetAside))
      `shouldBe` sort (map toCardCode [Enemies.captainSahir, Enemies.naasiyah])
    (.placement) <$> locationOf Locations.helmOfTheStormcaller `shouldReturn` StagingArea
    (.placement) <$> enemyOf Enemies.umbarRaider `shouldReturn` StagingArea

  -- \* Take the Ship!

  it "takes no progress until Helm of the Stormcaller is in the victory display" . thingTest $ \_ -> do
    q <- putQuestStage Quests.takeTheShip
    run $ Flip q
    run $ PlaceTokens GameSource q Progress 3
    getsGame (fmap (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` Just 0
    staged Locations.helmOfTheStormcaller
    helm <- locationOf Locations.helmOfTheStormcaller
    run $ AddToVictoryDisplay (toTarget helm.id)
    run $ PlaceTokens GameSource q Progress 3
    getsGame (fmap (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` Just 3

  it "adds a resource to Captain Sahír and Na'asiyah each round they are out of play, given them, indestructible, as Helm of the Stormcaller becomes active" . thingTest $ \pid -> do
    setAside [Enemies.captainSahir, Enemies.naasiyah]
    q <- putQuestStage Quests.takeTheShip
    run $ Flip q
    endOfRound
    endOfRound
    staged Locations.helmOfTheStormcaller
    helm <- locationOf Locations.helmOfTheStormcaller
    travelTo helm.id
    passActions pid
    sahir <- enemyOf Enemies.captainSahir
    naasiyah <- enemyOf Enemies.naasiyah
    map (countTokens Resource . enemyTokens) [sahir, naasiyah] `shouldBe` [2, 2]
    -- Helm of the Stormcaller gets a quest point for each resource on a Raider enemy
    getsGame (fmap (.placement) . find ((== helm.id) . toId) . toList . (.entities.locations)) `shouldReturn` Just ActiveLocation
    elem (GainKeyword Indestructible) <$> getModifiers (EnemyTarget sahir.enemyId) `shouldReturn` True

  -- \* Grapple

  it "grapples the location its parenthetical picks: not in the staging area, immune, +2 threat to the location, until it becomes active" . thingTest $ \_ -> do
    staged Locations.crewQuarters
    staged Locations.quarterDeck
    _ <- setEncounterDeck [Enemies.graspingArm]
    run RevealEncounterCard
    quarter <- locationOf Locations.quarterDeck
    arm <- enemyOf Enemies.graspingArm
    arm.placement `shouldBe` AttachedTo (LocationTarget quarter.locationId)
    select EnemyInStagingArea `shouldReturn` []
    elem ImmuneToPlayerCardEffects <$> getModifiers (EnemyTarget arm.enemyId) `shouldReturn` True
    getLocationThreat quarter.locationId `shouldReturn` 6
    travelTo quarter.locationId
    (.placement) <$> enemyOf Enemies.graspingArm `shouldReturn` StagingArea
    getLocationThreat quarter.locationId `shouldReturn` 4

  it "lets go of a location as it is explored" . thingTest $ \_ -> do
    staged Locations.cargoHold
    _ <- setEncounterDeck [Enemies.giantArm]
    run RevealEncounterCard
    hold <- locationOf Locations.cargoHold
    (.placement) <$> enemyOf Enemies.giantArm `shouldReturn` AttachedTo (LocationTarget hold.locationId)
    run $ PlaceTokens GameSource (toTarget hold.locationId) Progress 2
    (.placement) <$> enemyOf Enemies.giantArm `shouldReturn` StagingArea

  it "stays in the staging area with no location to grapple" . thingTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.thrashingArm]
    run RevealEncounterCard
    (.placement) <$> enemyOf Enemies.thrashingArm `shouldReturn` StagingArea

  it "grapples each enemy From the Depths discards" . thingTest $ \_ -> do
    staged Locations.forecastleDeck
    _ <- setEncounterDeck [Treacheries.fromTheDepths, Enemies.crushingArm, Locations.cargoHold, Enemies.eyeOfTheBeast]
    run RevealEncounterCard
    resolveRevealed
    forecastle <- locationOf Locations.forecastleDeck
    (.placement) <$> enemyOf Enemies.crushingArm `shouldReturn` AttachedTo (LocationTarget forecastle.locationId)
    sort . map toCardCode <$> getEncounterDiscard
      `shouldReturn` sort (map toCardCode [Treacheries.fromTheDepths, Enemies.eyeOfTheBeast, Locations.cargoHold])

  it "makes Crushing Arm attack the first player as it lets go" . thingTest $ \pid -> do
    staged Locations.crewQuarters
    _ <- setEncounterDeck [Enemies.crushingArm]
    run RevealEncounterCard
    crew <- locationOf Locations.crewQuarters
    travelTo crew.locationId
    arm <- enemyOf Enemies.crushingArm
    arm.placement `shouldBe` StagingArea
    -- its attack waits on the player's defenders
    isJust <$> getQuestion pid `shouldReturn` True

  it "has the Thing attack the player Eye of the Beast is engaged with, from the staging area" . thingTest $ \pid -> do
    staged Enemies.theThingInTheDepths
    staged Enemies.eyeOfTheBeast
    eye <- enemyOf Enemies.eyeOfTheBeast
    run $ EngageEnemy pid eye.enemyId
    thing <- enemyOf Enemies.theThingInTheDepths
    thing.placement `shouldBe` StagingArea
    select (EnemyEngagedWith $ PlayerWithId pid) >>= (`shouldMatchList'` [eye.enemyId, thing.enemyId])

  -- \* Change of Plans

  it "flips Sahír and Na'asiyah, removes the Corsair Raiders, brings in the Thing, and advances to Defend the Ship!" . thingTest $ \pid -> do
    staged Enemies.captainSahir
    staged Enemies.naasiyah
    staged Enemies.umbarRaider
    thing <- genEncounterCard Enemies.theThingInTheDepths
    arms <- traverse genEncounterCard [Enemies.thrashingArm, Enemies.giantArm, Enemies.crushingArm]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ thing : arms)
    deck <- setEncounterDeck [Enemies.corsairInfiltrator, Locations.cargoHold]
    stage3 <- genEncounterCard Quests.defendTheShip
    g' <- getGame
    putGame $ g' & scenarioL %~ overAttrs (questDeckL .~ [stage3])
    q <- putQuestStage Quests.changeOfPlans
    run $ Flip q
    resolveWhenRevealed pid
    allies <- getsGame (map toCardCode . toList . (.entities.allies))
    sort allies `shouldBe` sort (map toCardCode [Objectives.captainSahirObjective, Objectives.naasiyahObjective])
    map enemyCardCode <$> enemies `shouldReturn` [toCardCode Enemies.theThingInTheDepths]
    resolveWhenRevealed pid
    current <- getActiveQuest
    toCardCode current `shouldBe` toCardCode Quests.defendTheShip
    s <- scenarioAttrs
    filter (`elem` map (.id) deck) (map (.id) $ s.scenarioEncounterDeck <> s.scenarioEncounterDiscard)
      `shouldBe` [c.id | c <- deck, toCardCode c /= toCardCode Enemies.corsairInfiltrator]
    s.scenarioSetAside `shouldBe` []

  -- \* Defend the Ship!

  it "grapples a Tentacle enemy more than the players as it is revealed" . thingTest $ \pid -> do
    staged Locations.crewQuarters
    _ <- defending pid [Enemies.eyeOfTheBeast, Enemies.thrashingArm, Locations.weatherDeck, Enemies.giantArm, Enemies.crushingArm]
    crew <- locationOf Locations.crewQuarters
    (.placement) <$> enemyOf Enemies.thrashingArm `shouldReturn` AttachedTo (LocationTarget crew.locationId)
    (.placement) <$> enemyOf Enemies.giantArm `shouldReturn` AttachedTo (LocationTarget crew.locationId)
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.crushingArm]

  it "lets the players travel while a location is active, returning it to the staging area" . thingTest $ \pid -> do
    _ <- defending pid []
    staged Locations.crewQuarters
    staged Locations.cargoHold
    crew <- locationOf Locations.crewQuarters
    hold <- locationOf Locations.cargoHold
    travelTo crew.locationId
    run $ BeginPhase TravelPhase
    passActions pid
    chooseTarget pid hold.locationId
    (.placement) <$> locationOf Locations.cargoHold `shouldReturn` ActiveLocation
    (.placement) <$> locationOf Locations.crewQuarters `shouldReturn` StagingArea

  it "loses the game at the end of the round with a Ship location at 9 threat" . thingTest $ \pid -> do
    _ <- defending pid []
    staged Locations.quarterDeck
    _ <- setEncounterDeck [Enemies.graspingArm, Enemies.graspingArm]
    run RevealEncounterCard
    run RevealEncounterCard
    endOfRound
    outcome `shouldReturn` Nothing
    _ <- setEncounterDeck [Enemies.graspingArm]
    run RevealEncounterCard
    endOfRound
    outcome `shouldReturn` Just Lost

  it "damages the Thing as a Creature enemy is destroyed, and wins the game as the Thing is" . thingTest $ \pid -> do
    _ <- defending pid []
    staged Enemies.theThingInTheDepths
    staged Enemies.eyeOfTheBeast
    eye <- enemyOf Enemies.eyeOfTheBeast
    run $ DealDamage GameSource (EnemyTarget eye.enemyId) 5
    thing <- enemyOf Enemies.theThingInTheDepths
    countTokens Damage thing.enemyTokens `shouldBe` 2
    outcome `shouldReturn` Nothing
    run $ DealDamage GameSource (EnemyTarget thing.enemyId) 9
    outcome `shouldReturn` Just Won
