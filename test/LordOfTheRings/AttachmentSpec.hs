module LordOfTheRings.AttachmentSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import TestImport

playOnTheodred :: PlayerId -> CardDef -> GameT ()
playOnTheodred pid def = do
  card <- putCardIntoHand pid def
  theodred <- getHeroWithCode "01002"
  run $ PlaceTokens GameSource (toTarget theodred) Resource 2
  run $ BeginPhase PlanningPhase
  chooseTarget pid card
  chooseTarget pid theodred

spec :: Spec
spec = describe "Attachments" do
  it "attach to the chosen hero" . gameTest $ \pid -> do
    playOnTheodred pid Attachments.stewardOfGondor
    theodred <- getHeroWithCode "01002"
    placements <- getsGame (map (.placement) . toList . (.entities.attachments))
    placements `shouldBe` [AttachedTo (toTarget theodred)]

  it "Celebrían's Stone gives the attached hero +2 willpower" . gameTest $ \pid -> do
    playOnTheodred pid Attachments.celebriansStone
    theodred <- getHeroWithCode "01002"
    getWillpower (toTarget theodred) `shouldReturn` 3

  it "leave play when the hero they are attached to does" . gameTest $ \pid -> do
    playOnTheodred pid Attachments.stewardOfGondor
    theodred <- getHeroWithCode "01002"
    run $ Discard (toTarget theodred)
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    sort . map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` ["01002", "01026"]

  it "cannot be played without something to attach to" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Attachments.forestSnare
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 3
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (toTarget card)
