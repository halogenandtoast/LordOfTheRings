module LordOfTheRings.AttachmentSpec (spec) where

import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier
import LordOfTheRings.Trait (Trait (Gondor))
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

-- | The one attachment in play, as a target.
steward :: GameT Target
steward = getsGame (toTarget . fromJustNote "no attachment" . headMay . toList . (.entities.attachments))

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
    playOnTheodred pid Attachments.celebriansStone
    theodred <- getHeroWithCode "01002"
    passActions pid
    run $ Discard (toTarget theodred)
    getsGame (length . (.entities.attachments)) `shouldReturn` 0
    sort . map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` ["01002", "01027"]

  it "cannot be played without something to attach to" . gameTest $ \pid -> do
    card <- putCardIntoHand pid Attachments.forestSnare
    beravor <- getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget beravor) Resource 3
    run $ BeginPhase PlanningPhase
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (toTarget card)

  describe "Steward of Gondor" do
    it "exhausts to add 2 resources to the attached hero" . gameTest $ \pid -> do
      playOnTheodred pid Attachments.stewardOfGondor
      chooseAbilityOf pid =<< steward
      theodred <- getHeroWithCode "01002"
      theodred.resources `shouldBe` 2
      steward <- getsGame (fromJustNote "no steward" . headMay . toList . (.entities.attachments))
      attachmentExhausted (toAttrs steward) `shouldBe` True

    it "cannot be used again while exhausted" . gameTest $ \pid -> do
      playOnTheodred pid Attachments.stewardOfGondor
      chooseAbilityOf pid =<< steward
      (offersAbilityOf pid =<< steward) `shouldReturn` False

    it "gives the attached hero the Gondor trait" . gameTest $ \pid -> do
      playOnTheodred pid Attachments.stewardOfGondor
      theodred <- getHeroWithCode "01002"
      getModifiers theodred >>= (`shouldSatisfy` elem (GainTrait Gondor))

    it "readies during the refresh phase" . gameTest $ \pid -> do
      playOnTheodred pid Attachments.stewardOfGondor
      chooseAbilityOf pid =<< steward
      passActions pid
      run $ BeginPhase RefreshPhase
      steward <- getsGame (fromJustNote "no steward" . headMay . toList . (.entities.attachments))
      attachmentExhausted (toAttrs steward) `shouldBe` False

  it "cannot play a second copy of a unique attachment" . gameTest $ \pid -> do
    first <- putCardIntoHand pid Attachments.celebriansStone
    second <- putCardIntoHand pid Attachments.celebriansStone
    theodred <- getHeroWithCode "01002"
    run $ PlaceTokens GameSource (toTarget theodred) Resource 4
    run $ BeginPhase PlanningPhase
    chooseTarget pid first
    chooseTarget pid theodred
    question <- getQuestion pid
    [t | Just q <- [question], TargetLabel t _ <- questionChoices q] `shouldSatisfy` notElem (toTarget second)
