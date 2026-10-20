module LordOfTheRings.SetupSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (toCardCode)
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Game.Public (publicGameFor)
import LordOfTheRings.Scenario.Types (easyModeL)
import TestImport

spec :: Spec
spec = describe "Setup" do
  it "sets up every seat at a multi-handed table, asking each about their hand" do
    seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", starterDeck)])
    env <- newGameEnv (newTableIn passageThroughMirkwood seats) (const $ pure ()) 1
    runGameT env do
      run Setup
      players <- traverse (getPlayer . (.id) . fst) (toList seats)
      map (length . (.hand)) players `shouldBe` [6, 6]
      map (.threat) players `shouldBe` [27, 29]
      [first] <- getsGame (Map.keys . (.question))
      chooseOptionMatching "keep the hand" first \case
        Label _ [] -> True
        _ -> False
      next <- getsGame (Map.keys . (.question))
      sort (first : next) `shouldBe` sort (map (.id) players)
  it "shows a player only the hands they may see, with the size of the rest" do
    seats <- traverse (fmap (either (error . show) id) . uncurry loadPlayer) (("one", testDeck) :| [("two", starterDeck)])
    env <- newGameEnv (newTableIn passageThroughMirkwood seats) (const $ pure ()) 1
    runGameT env do
      run Setup
      [one, two] <- getsGame (.playerOrder)
      seen <- getsGame (publicGameFor (== one))
      let player pid = case seen of
            Object o | Just (Object ps) <- KeyMap.lookup "players" o, Just (Object p) <- KeyMap.lookup (fromString $ show pid) ps -> p
            _ -> error "no player"
          field k pid = KeyMap.lookup k (player pid)
      field "hand" two `shouldBe` Just (Array mempty)
      field "handSize" two `shouldBe` Just (Number 6)
      (length <$> (\case Just (Array cards) -> Just cards; _ -> Nothing) (field "hand" one)) `shouldBe` Just 6

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

  it "in easy mode, leaves the difficult copies out and gives each hero a resource" do
    gameTestIn (overAttrs (easyModeL .~ True) passageThroughMirkwood) testDeck \_ -> do
      run Setup
      deck <- getEncounterDeck
      let copies code = length $ filter ((== code) . toCardCode) deck
      -- Hummerhorns gone, Dol Guldur Beastmaster and The Necromancer's Reach thinned,
      -- Enchanted Stream untouched
      map copies ["01075", "01091", "01093", "01095"] `shouldBe` [0, 1, 1, 2]
      heroes <- getsGame (Map.keys . (.entities.heroes))
      traverse getHeroResources heroes `shouldReturn` [1, 1, 1]

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
