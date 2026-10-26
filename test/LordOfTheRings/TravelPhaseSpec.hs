module LordOfTheRings.TravelPhaseSpec (spec) where

import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import TestImport

placements :: GameT [Placement]
placements = getsGame (map (locationPlacement . toAttrs) . toList . (.entities.locations))

-- | Gimli, Thalin and Théodred, none with an Action, so an empty hand leaves nothing to do.
idleDeck :: DeckList
idleDeck = testDeck {heroes = ["01004", "01006", "01002"]}

emptyHand :: PlayerId -> GameT ()
emptyHand pid = putGame . (playersL . ix pid . handL .~ []) =<< getGame

-- | The open question's prompt, then its labelled choices.
passLabels :: PlayerId -> GameT [Text]
passLabels pid =
  getQuestion pid <&> \case
    Just (QuestionLabel prompt _ _ q) -> prompt : [l | Label l _ <- questionChoices q]
    _ -> []

spec :: Spec
spec = describe "Travel phase" do
  it "opens an action window before travelling, without a Travel Action" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldBe` "$label.takeAction"

  it "does not name the encounter phase while travel is still to come" . gameTestWith idleDeck $ \pid -> do
    flipFirstQuest pid
    emptyHand pid
    run $ BeginPhase TravelPhase
    passLabels pid `shouldReturn` ["$label.noRemainingActions", "$label.pass"]

  it "names the encounter phase when no travel follows" . gameTestWith idleDeck $ \pid -> do
    flipFirstQuest pid
    emptyHand pid
    location <- getsGame (fromJustNote "no location" . headMay . toList . (.entities.locations))
    run $ PlaceLocation (toId location) ActiveLocation
    run $ BeginPhase TravelPhase
    passLabels pid `shouldReturn` ["$label.noRemainingActions", "$label.proceedTo.encounter"]

  it "offers every location in the staging area" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    passActions pid
    Just question <- getQuestion pid
    length [() | TargetLabel {} <- questionChoices question] `shouldBe` 1

  it "makes the chosen location active" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    passActions pid
    chooseOptionMatching "travel" pid \case
      TargetLabel {} -> True
      _ -> False
    placements `shouldReturn` [ActiveLocation]

  it "leaves every location where it was when declining" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase TravelPhase
    passActions pid
    chooseLabel "$label.doNotTravel" pid
    placements `shouldReturn` [StagingArea]

  it "does not ask while a location is already active" . gameTest $ \pid -> do
    flipFirstQuest pid
    location <- getsGame (fromJustNote "no location" . headMay . toList . (.entities.locations))
    run $ PlaceLocation (toId location) ActiveLocation
    run $ BeginPhase TravelPhase
    passActions pid
    getQuestion pid `shouldReturn` Nothing
