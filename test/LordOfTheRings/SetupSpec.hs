module LordOfTheRings.SetupSpec (spec) where

import LordOfTheRings.Card (toCardCode)
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

  it "flips Flies and Spiders and puts its setup cards in staging" . gameTest $ \pid -> do
    flipFirstQuest pid
    enemies <- getsGame (toList . (.entities.enemies))
    locations <- getsGame (toList . (.entities.locations))
    map toCardCode enemies `shouldBe` ["01096"]
    map toCardCode locations `shouldBe` ["01099"]

  it "keeps the quest stages out of the encounter deck" . gameTest $ \_ -> do
    run Setup
    deck <- getEncounterDeck
    filter (`elem` ["01119", "01120", "01121", "01122"]) (map toCardCode deck) `shouldBe` []

  it "offers a mulligan that redraws a fresh hand of 6" . gameTest $ \pid -> do
    run Setup
    chooseOptionMatching "mulligan" pid \case
      Label _ [Mulligan _] -> True
      _ -> False
    player <- getPlayer pid
    length player.hand `shouldBe` 6
    getQuestion pid >>= (`shouldSatisfy` isJust) -- straight on to flipping the quest
