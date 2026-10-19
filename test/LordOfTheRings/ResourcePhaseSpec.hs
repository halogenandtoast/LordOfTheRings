module LordOfTheRings.ResourcePhaseSpec (spec) where

import LordOfTheRings.Phase
import TestImport

spec :: Spec
spec = describe "Resource phase" do
  it "adds 1 resource to each hero" . gameTest $ \_ -> do
    run $ BeginPhase ResourcePhase
    heroes <- getsGame (toList . (.entities.heroes))
    map (.resources) heroes `shouldBe` [1, 1, 1]

  it "draws 1 card for each player" . gameTest $ \pid -> do
    flipFirstQuest pid
    handSize <- length . (.hand) <$> getPlayer pid
    run $ BeginPhase ResourcePhase
    length . (.hand) <$> getPlayer pid `shouldReturn` handSize + 1
