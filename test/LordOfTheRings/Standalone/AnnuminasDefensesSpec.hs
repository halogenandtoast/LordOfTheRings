module LordOfTheRings.Standalone.AnnuminasDefensesSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (locationTokens)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Phase (Phase (TravelPhase, QuestPhase))
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (cityStrength)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioVictoryDisplay))
import LordOfTheRings.Standalone.AnnuminasCitySpec (stageCity, setReplica)
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import TestImport

resources :: LocationId -> GameT Int
resources lid = countTokens Resource . locationTokens . toAttrs <$> getLocation lid

spec :: Spec
spec = describe "Annúminas defensive siege locations" do
  for_ [StagingArea, ActiveLocation] $ \placement -> do
    it ("increments Battering Ram before calculating its growing city damage: " <> show placement) . gameTest $ \pid -> do
      _ <- stageCity 20
      ram <- stageLocation Locations.batteringRam
      run $ PlaceLocation ram placement
      getLocationThreat ram `shouldReturn` 1
      run $ UseCardAbility pid (LocationSource ram) 1 []
      resources ram `shouldReturn` 1
      getLocationThreat ram `shouldReturn` 2
      cityStrength `shouldReturn` 18
      run $ UseCardAbility pid (LocationSource ram) 1 []
      resources ram `shouldReturn` 2
      getLocationThreat ram `shouldReturn` 3
      cityStrength `shouldReturn` 15

  for_ [Locations.cityWallAnnuminas, Locations.batteringRam] $ \def -> do
    it ("is immune to player-card progress and loses its printed effects when blank: " <> show (toCardCode def)) . gameTest $ \pid -> do
      lid <- stageLocation def
      hero <- heroOf pid "01012"
      getModifiers lid >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ PlaceTokens (case hero of HeroTarget hid -> HeroSource hid; _ -> GameSource) (LocationTarget lid) Progress 1
      (.progress) <$> getLocation lid `shouldReturn` 0
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
      getModifiers lid >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)
      run $ PlaceTokens (case hero of HeroTarget hid -> HeroSource hid; _ -> GameSource) (LocationTarget lid) Progress 1
      (.progress) <$> getLocation lid `shouldReturn` 1
      getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo lid
      (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "reveals one encounter per player when traveling to Battering Ram" . twoPlayerTest testDeck $ \_ _ -> do
    ram <- stageLocation Locations.batteringRam
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run $ TravelTo ram
    getsGame (length . (.entities.locations)) `shouldReturn` 3
    (.placement) <$> getLocation ram `shouldReturn` ActiveLocation

  it "reveals only one encounter when traveling to City Wall, even with two players" . twoPlayerTest testDeck $ \_ _ -> do
    wall <- stageLocation Locations.cityWallAnnuminas
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    run $ TravelTo wall
    getsGame (length . (.entities.locations)) `shouldReturn` 2
    (.placement) <$> getLocation wall `shouldReturn` ActiveLocation

  it "discards City Wall on its third resource, damages the city, and reveals rather than merely stages its encounter" . gameTest $ \pid -> do
    run $ DoStep 2 Setup
    _ <- stageCity 20
    wall <- stageLocation Locations.cityWallAnnuminas
    _ <- setEncounterDeck [Enemies.greenwayFootpad]
    replicateM_ 2 $ run $ UseCardAbility pid (LocationSource wall) 1 []
    resources wall `shouldReturn` 2
    cityStrength `shouldReturn` 20
    run $ UseCardAbility pid (LocationSource wall) 1 []
    resolveRevealed
    chooseLabel "$label.prancingPony.raiseEachThreat2" pid
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    cityStrength `shouldReturn` 16
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.greenwayFootpad]
    (.threat) <$> getPlayer pid `shouldReturn` 29

  it "keeps each City Wall's resource counter separate" . gameTest $ \pid -> do
    _ <- stageCity 20
    former <- stageLocation Locations.cityWallAnnuminas
    latter <- stageLocation Locations.cityWallAnnuminas
    _ <- setEncounterDeck $ replicate 3 Locations.oldForestRoad
    replicateM_ 2 $ run $ UseCardAbility pid (LocationSource former) 1 []
    run $ UseCardAbility pid (LocationSource latter) 1 []
    resources former `shouldReturn` 2
    resources latter `shouldReturn` 1
    run $ UseCardAbility pid (LocationSource former) 1 []
    cityStrength `shouldReturn` 16
    resources latter `shouldReturn` 1

  it "preserves Battering Ram's accumulated resources through save and reload" . gameTest $ \pid -> do
    _ <- stageCity 20
    ram <- stageLocation Locations.batteringRam
    run $ UseCardAbility pid (LocationSource ram) 1 []
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    run $ UseCardAbility pid (LocationSource ram) 1 []
    resources ram `shouldReturn` 2
    cityStrength `shouldReturn` 15

  it "contributes its damage from a team without the city card" . gameTest $ \pid -> do
    setReplica []
    ram <- stageLocation Locations.batteringRam
    run $ UseCardAbility pid (LocationSource ram) 1 []
    cityStrength `shouldReturn` 48
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.deltaAmount) replica.epicPendingDeltas `shouldBe` [-2]

  it "puts an explored Battering Ram in the victory display for five points" . gameTest $ \_ -> do
    ram <- stageLocation Locations.batteringRam
    run $ PlaceLocation ram ActiveLocation
    run $ PlaceTokens GameSource (LocationTarget ram) Progress 5
    getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.batteringRam]
    getsGame ((.victoryPoints) . toAttrs . (.scenario)) `shouldReturn` 5

  it "requires the full reveal cost and does not reset an empty deck during travel" . twoPlayerTest testDeck $ \_ _ -> do
    ram <- stageLocation Locations.batteringRam
    current <- getGame
    putGame $ current & phaseL .~ TravelPhase
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.oldForestRoad
    getModifiers ram >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ TravelTo ram
    (.placement) <$> getLocation ram `shouldReturn` StagingArea
    _ <- setEncounterDeck $ replicate 2 Locations.oldForestRoad
    getModifiers ram >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

  it "counts the discard supply only when the encounter deck can reset, and responds immediately to a new reset prohibition" . twoPlayerTest testDeck $ \_ _ -> do
    ram <- stageLocation Locations.batteringRam
    current <- getGame
    putGame $ current & phaseL .~ QuestPhase
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.oldForestRoad
    getModifiers ram >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ CreateEffect $ Effect GameSource GameTarget [EncounterDeckDoesNotReset] UntilEndOfRound
    getModifiers ram >>= (`shouldSatisfy` elem CannotBeTraveledTo)
