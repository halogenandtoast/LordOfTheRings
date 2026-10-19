module LordOfTheRings.SetupSpec (spec) where

import TestImport

spec :: Spec
spec = describe "Setup" do
  it "sets starting threat to the total of the heroes' threat costs" . gameTest $ \pid -> do
    run Setup
    (.threat) <$> getPlayer pid `shouldReturn` 27

  it "draws a starting hand of 6 cards" . gameTest $ \pid -> do
    deckSize <- length . (.deck) <$> getPlayer pid
    run Setup
    player <- getPlayer pid
    length player.hand `shouldBe` 6
    length player.deck `shouldBe` deckSize - 6
