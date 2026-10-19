module LordOfTheRings.TravelPhaseSpec (spec) where

import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import TestImport

placements :: GameT [Placement]
placements = getsGame (map (locationPlacement . toAttrs) . toList . (.entities.locations))

spec :: Spec
spec = describe "Travel phase" do
  it "offers every location in the staging area" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    Just question <- getQuestion pid
    length [() | TargetLabel {} <- questionChoices question] `shouldBe` 1

  it "makes the chosen location active" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    chooseOptionMatching "travel" pid \case
      TargetLabel {} -> True
      _ -> False
    placements `shouldReturn` [ActiveLocation]

  it "leaves every location where it was when declining" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    chooseLabel "$label.doNotTravel" pid
    placements `shouldReturn` [StagingArea]

  it "does not ask while a location is already active" . gameTest $ \pid -> do
    flipFirstQuest pid
    location <- getsGame (fromJustNote "no location" . headMay . toList . (.entities.locations))
    run $ PlaceLocation (toId location) ActiveLocation
    run $ BeginPhase TravelPhase
    getQuestion pid `shouldReturn` Nothing
