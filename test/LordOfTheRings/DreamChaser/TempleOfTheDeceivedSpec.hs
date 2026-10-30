module LordOfTheRings.DreamChaser.TempleOfTheDeceivedSpec (spec) where

import LordOfTheRings.Card (CardType (LocationType), cdCardType, toCardDef)
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Helpers.Window (travelableLocations)
import LordOfTheRings.Location (LocationAttrs (..), isUncharted)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Scenario (lookupScenario, templeOfTheDeceived)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), victoryDisplayL)
import TestImport

templeTest :: (PlayerId -> GameT ()) -> IO ()
templeTest = gameTestIn templeOfTheDeceived testDeck

locations :: GameT [LocationAttrs]
locations = getsGame (map toAttrs . toList . (.entities.locations))

-- | The location in that place on the Island Map.
mapAt :: Pos -> GameT LocationAttrs
mapAt pos = fromJustNote ("nothing at " <> show pos) . find ((== Just pos) . locationPosition) <$> locations

-- | Lay out an Island Map of the given cards, each in its place, Uncharted side faceup.
layMap :: [(Pos, CardDef)] -> GameT ()
layMap cards = run . PrepareIslandMap =<< traverse (traverse genEncounterCard) cards

-- | Make the location at that place active, as the players do at setup.
activate :: Pos -> GameT LocationId
activate pos = do
  lid <- locationId <$> mapAt pos
  run $ PlaceLocation lid ActiveLocation
  pure lid

placementAt :: Pos -> GameT Placement
placementAt = fmap locationPlacement . mapAt

gateKey :: GameT ObjectiveAttrs
gateKey = getsGame (fromJustNote "no Gate Key" . find ((== toCardCode Objectives.gateKey) . objectiveCardCode) . map toAttrs . toList . (.entities.objectives))

guardian :: GameT (Maybe EnemyAttrs)
guardian = getsGame (find ((== toCardCode Enemies.templeGuardian) . toCardCode) . map toAttrs . toList . (.entities.enemies))

-- | A small map: Flooded Ruins top-left, Winding Caverns to its right, Cursed Temple below
-- it, and Jagged Cliffs and Jungle Path further on.
smallMap :: GameT ()
smallMap =
  layMap
    [ (Pos 0 0, Locations.floodedRuins)
    , (Pos 1 0, Locations.windingCaverns)
    , (Pos 2 0, Locations.cursedTemple)
    , (Pos 3 0, Locations.drownedGraves)
    , (Pos 4 0, Locations.theGrottosEntrance)
    , (Pos 0 1, Locations.cursedTemple)
    , (Pos 1 1, Locations.jaggedCliffs)
    , (Pos 2 1, Locations.floodedRuins)
    , (Pos 3 1, Locations.windingCaverns)
    , (Pos 1 2, Locations.junglePath)
    ]

spec :: Spec
spec = describe "Temple of the Deceived" do
  it "is a scenario the game can be dealt" . templeTest $ \_ ->
    isJust (lookupScenario "temple-of-the-deceived") `shouldBe` True

  it "lays out the Island Map, starts at a flipped left corner, and sets up Sahír, Na'asiyah, the Guardian and the Gate Key" . templeTest $ \pid -> do
    flipFirstQuest pid
    corner <- mapAt (Pos 0 0)
    chooseTarget pid (LocationTarget corner.locationId)
    ls <- locations
    length (filter (isJust . locationPosition) ls) `shouldBe` 15
    let lostIslands = [l | l <- ls, Just p <- [locationPosition l], p.column < 4]
        temples = [l | l <- ls, Just p <- [locationPosition l], p.column == 4]
    length lostIslands `shouldBe` 12
    map locationCardCode temples `shouldBe` replicate 3 (toCardCode Locations.templeOfTheDeceived)
    placementAt (Pos 0 0) `shouldReturn` ActiveLocation
    -- both left corners turned over, the bottom one off in the map
    for_ [Pos 0 0, Pos 0 2] \p -> (isUncharted <$> mapAt p) `shouldReturn` False
    placementAt (Pos 0 2) `shouldReturn` OnIslandMap
    placementAt (Pos 3 1) `shouldReturn` OnIslandMap
    allies <- getsGame (map toCardCode . toList . (.entities.allies))
    sort allies `shouldBe` sort (map toCardCode [Objectives.captainSahirObjective, Objectives.naasiyahObjective])
    g <- fromJustNote "no Guardian" <$> guardian
    enemyPlacement g `shouldBe` StagingArea
    (objectivePlacement <$> gateKey) `shouldReturn` AttachedTo (EnemyTarget (enemyId g))
    deck <- getEncounterDeck
    filter ((== LocationType) . cdCardType . toCardDef) deck `shouldBe` []
    filter ((`elem` map toCardCode [Enemies.templeGuardian, Objectives.gateKey]) . toCardCode) deck `shouldBe` []

  -- \* The Island Map

  it "puts the locations orthogonally adjacent to the active location in the staging area, and only them" . templeTest $ \_ -> do
    smallMap
    _ <- activate (Pos 0 0)
    (locationCardCode <$> mapAt (Pos 0 0)) `shouldReturn` toCardCode Locations.floodedRuins
    placementAt (Pos 1 0) `shouldReturn` StagingArea
    placementAt (Pos 0 1) `shouldReturn` StagingArea
    placementAt (Pos 1 1) `shouldReturn` OnIslandMap
    placementAt (Pos 2 0) `shouldReturn` OnIslandMap
    -- two Lost Islands, and Flooded Ruins adding its own threat while active
    getStagingThreat `shouldReturn` 7

  it "travels only once the active location has full progress, to an adjacent location, which stays explored in its place" . templeTest $ \_ -> do
    smallMap
    start <- activate (Pos 0 0)
    travelableLocations `shouldReturn` []
    run $ PlaceTokens GameSource (LocationTarget start) Progress 5
    placementAt (Pos 0 0) `shouldReturn` ActiveLocation
    right <- locationId <$> mapAt (Pos 1 0)
    below <- locationId <$> mapAt (Pos 0 1)
    sort <$> travelableLocations `shouldReturn` sort [right, below]
    run $ TravelTo right
    placementAt (Pos 1 0) `shouldReturn` ActiveLocation
    (locationCardCode <$> mapAt (Pos 1 0)) `shouldReturn` toCardCode Locations.windingCaverns
    old <- mapAt (Pos 0 0)
    (locationPlacement old, old.progress, locationCardCode old) `shouldBe` (StagingArea, 0, toCardCode Locations.floodedRuins)
    placementAt (Pos 2 0) `shouldReturn` StagingArea
    placementAt (Pos 1 1) `shouldReturn` StagingArea
    placementAt (Pos 0 1) `shouldReturn` OnIslandMap

  it "lets the players travel up to 3 locations away from Winding Caverns" . templeTest $ \_ -> do
    smallMap
    caverns <- activate (Pos 1 0)
    run $ PlaceTokens GameSource (LocationTarget caverns) Progress 6
    reachable <- travelableLocations
    -- two away, and three, but not the Temple without the Gate Key
    two <- locationId <$> mapAt (Pos 1 2)
    three <- locationId <$> mapAt (Pos 3 1)
    temple <- locationId <$> mapAt (Pos 4 0)
    (two `elem` reachable, three `elem` reachable, temple `elem` reachable) `shouldBe` (True, True, False)

  it "keeps the location to the right of Jagged Cliffs out of the staging area" . templeTest $ \_ -> do
    smallMap
    _ <- activate (Pos 1 1)
    placementAt (Pos 2 1) `shouldReturn` OnIslandMap
    placementAt (Pos 0 1) `shouldReturn` StagingArea
    placementAt (Pos 1 0) `shouldReturn` StagingArea

  -- \* The Gate Key and the Temple

  it "returns the Gate Key to the staging area as its Guardian is destroyed, for a hero to claim it" . templeTest $ \pid -> do
    flipFirstQuest pid
    chooseTarget pid . LocationTarget . locationId =<< mapAt (Pos 0 0)
    g <- fromJustNote "no Guardian" <$> guardian
    run $ Destroy GameSource (EnemyTarget (enemyId g))
    getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.templeGuardian]
    key <- gateKey
    objectivePlacement key `shouldBe` StagingArea
    hero <- getHeroWithCode "01012"
    run $ UseCardAbility pid (ObjectiveSource (objectiveId key)) 1 []
    chooseTarget pid hero
    (objectivePlacement <$> gateKey) `shouldReturn` AttachedTo (toTarget hero)
    (heroExhausted . toAttrs <$> getHeroWithCode "01012") `shouldReturn` True

  it "opens the Temple only to the Gate Key, exhausting it, and wins at the 8th progress on The Grotto's Entrance" . templeTest $ \pid -> do
    smallMap
    hero <- getHeroWithCode "01012"
    key <- genEncounterCard Objectives.gateKey
    run $ AddToStaging key
    keyId <- objectiveId <$> gateKey
    run $ ClaimObjective keyId (toTarget hero)
    -- the Guardian waits in the victory display
    card <- genEncounterCard Enemies.templeGuardian
    g0 <- getGame
    putGame $ g0 & scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard card])
    beside <- activate (Pos 3 0)
    run $ PlaceTokens GameSource (LocationTarget beside) Progress 2
    temple <- locationId <$> mapAt (Pos 4 0)
    elem temple <$> travelableLocations `shouldReturn` True
    run . Exhaust . toTarget =<< gateKey
    elem temple <$> travelableLocations `shouldReturn` False
    run . Ready . toTarget =<< gateKey
    run $ TravelTo temple
    (objectiveExhausted <$> gateKey) `shouldReturn` True
    (locationCardCode <$> mapAt (Pos 4 0)) `shouldReturn` toCardCode Locations.theGrottosEntrance
    (fmap enemyPlacement <$> guardian) `shouldReturn` Just (EngagedWith pid)
    run $ PlaceTokens GameSource (LocationTarget temple) Progress 7
    getsGame (.outcome) `shouldReturn` Nothing
    run $ PlaceTokens GameSource (LocationTarget temple) Progress 1
    getsGame (.outcome) `shouldReturn` Just Won

  it "lets the first player place Searching the Island's progress on locations in the staging area instead" . templeTest $ \pid -> do
    smallMap
    _ <- activate (Pos 0 0)
    q <- putQuestStage Quests.searchingTheIsland
    g <- getGame
    putGame $ g & entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})
    island <- locationId <$> mapAt (Pos 1 0)
    run $ PlaceTokens GameSource q Progress 2
    chooseTarget pid (LocationTarget island)
    chooseLabel "$label.searchingTheIsland.onQuest" pid
    ((.progress) <$> mapAt (Pos 1 0)) `shouldReturn` 1
    ((.progress) <$> getActiveQuest) `shouldReturn` 1
