module LordOfTheRings.SerializationSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Phase
import TestImport

roundTrip :: (ToJSON a, FromJSON a) => a -> Either String a
roundTrip = eitherDecode . encode

spec :: Spec
spec = describe "Serialization" do
  it "round-trips a game in progress" . gameTest $ \pid -> do
    flipFirstQuest pid
    card <- putCardIntoHand pid Attachments.celebriansStone
    theodred <- getHeroWithCode "01002"
    run $ PlaceTokens GameSource (toTarget theodred) Resource 2
    run $ BeginPhase PlanningPhase
    chooseTarget pid card
    chooseTarget pid theodred
    g <- getGame
    case roundTrip g of
      Left err -> liftIO $ expectationFailure err
      Right g' -> toJSON g' `shouldBe` toJSON g

  it "round-trips the message queue, questions included" . gameTest $ \_ -> do
    run Setup
    queue <- peekQueue
    question <- getsGame (.question)
    fmap toJSON (roundTrip queue) `shouldBe` Right (toJSON queue)
    fmap toJSON (roundTrip question) `shouldBe` Right (toJSON question)

  it "keeps playing from a decoded game" . gameTest $ \pid -> do
    run Setup
    g <- getGame
    either error putGame (roundTrip g)
    chooseLabel "$label.keepHand" pid
    chooseOptionMatching "flip the quest" pid (const True)
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
