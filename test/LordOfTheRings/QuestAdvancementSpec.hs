module LordOfTheRings.QuestAdvancementSpec (spec) where

import LordOfTheRings.Outcome
import TestImport

completeQuest :: GameT ()
completeQuest = do
  quest <- getActiveQuest
  run $ PlaceTokens GameSource (toTarget quest) Progress quest.questPoints

spec :: Spec
spec = describe "Quest advancement" do
  it "replaces a completed stage with the next one, flipped and without progress" . gameTest $ \pid -> do
    flipFirstQuest pid
    completeQuest
    quest <- getActiveQuest
    toCardCode quest `shouldBe` "01120"
    quest.flipped `shouldBe` True
    quest.progress `shouldBe` 0
    getsGame (.outcome) `shouldReturn` Nothing

  it "lets the players choose which path to take at A Fork in the Road" . gameTest $ \pid -> do
    flipFirstQuest pid
    completeQuest
    completeQuest
    chooseLabel "A Chosen Path" pid
    toCardCode <$> getActiveQuest `shouldReturn` "01121"

  it "wins the game when the final stage is completed" . gameTest $ \pid -> do
    flipFirstQuest pid
    completeQuest
    completeQuest
    chooseOptionMatching "take Beorn's Path" pid \case
      Label _ [AdvanceToQuest card] -> toCardCode card == "01122"
      _ -> False
    completeQuest
    getsGame (.outcome) `shouldReturn` Just Won
