module Main (main) where

import CardDb (CardDb, loadCardDb)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Player.Types (Player, playerMessengerOfTheKingAlly, playerDeck, playerContract)
import Decks (importDeck, previewDeck, describeImportError)
import LordOfTheRings.Card (CardCode (..))
import LordOfTheRings.Decklist
import LordOfTheRings.Prelude hiding (preview)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment qualified as Env
import Test.Hspec

-- Factual card lists captured from RingsDB's public decklist API, 2026-11-05.
-- The two requested decks have no synthetic heroes; 14632 exercises a real MotKA export.
fixture :: Int -> IO Value
fixture ident = either (error . show) pure =<< eitherDecodeFileStrict' ("test-decks/fixtures/ringsdb-" <> show ident <> ".json")

preview :: Manager -> CardDb -> Value -> IO Value
preview manager db deck = previewRequest manager db (object ["deck" .= deck])

previewRequest :: Manager -> CardDb -> Value -> IO Value
previewRequest manager db value = do
  request <- case fromJSON value of
    Aeson.Error why -> error why
    Aeson.Success x -> pure x
  imported <- importDeck manager db request
  either (error . unpack . describeImportError) (pure . toJSON . previewDeck db) imported

field :: FromJSON a => Key -> Value -> a
field key value = either error id $ parseEither (withObject "field" (.: key)) value

playable :: Value -> IO DeckList
playable value = do
  field @(Maybe Text) "problem" value `shouldBe` Nothing
  let result = field @(Maybe DeckList) "list" value
  result `shouldSatisfy` isJust
  pure $ fromJustNote "unplayable deck" result

main :: IO ()
main = do
  db <- either error pure =<< loadCardDb "frontend/public/data/cards.json"
  manager <- newManager tlsManagerSettings
  live <- (== Just "1") <$> Env.lookupEnv "MOTKA_LIVE_TESTS"
  hspec $ describe "RingsDB Messenger of the King imports" do
    when live $ for_ [19545, 15839, 14632] \ident ->
      it ("imports live RingsDB link " <> show ident <> " identically to its captured export") do
        result <- previewRequest manager db (object ["url" .= ("https://ringsdb.com/decklist/view/" <> tshow (ident :: Int))])
        dl <- playable result
        expected <- playable =<< preview manager db =<< fixture ident
        dl `shouldBe` expected

    for_ [(19545, ["04128", "142002"], 50), (15839, ["04128", "10140"], 50)] \(ident, expectedHeroes, expectedCount) ->
      it ("imports requested deck " <> show ident <> " without changing its flexible ally choice") do
        result <- preview manager db =<< fixture ident
        dl <- playable result
        dl.heroes `shouldBe` expectedHeroes
        dl.messengerOfTheKingAlly `shouldBe` Nothing
        dl.contract `shouldBe` Just "22134"
        sum (map snd dl.cards) `shouldBe` expectedCount
        field @Int "cardCount" result `shouldBe` expectedCount
        lookup "01014" dl.cards `shouldBe` Just 1
        (player, heroes) <- either (error . show) pure =<< loadPlayer "RingsDB" dl
        length heroes `shouldBe` 2
        length (playerDeck player) `shouldBe` expectedCount
        playerContract player `shouldSatisfy` isJust

    it "restores MotKA Rosie exactly once from a real export and retains the intended selection" do
      result <- preview manager db =<< fixture 14632
      dl <- playable result
      dl.heroes `shouldBe` ["02025", "141004"]
      dl.messengerOfTheKingAlly `shouldBe` Just "146005"
      dl.contract `shouldBe` Just "22134"
      lookup "146005" dl.cards `shouldBe` Just 1
      sum (map snd dl.cards) `shouldBe` 53
      field @Int "cardCount" result `shouldBe` 53
      normalizeMessengerOfTheKing dl `shouldBe` dl
      fromJSON (toJSON dl) `shouldBe` Aeson.Success dl

    it "keeps extra ordinary ally copies in addition to the synthetic hero's copy" do
      let deck = object ["heroes" .= object ["01012" .= (1 :: Int), "9901014" .= (1 :: Int)], "slots" .= object ["01012" .= (1 :: Int), "9901014" .= (1 :: Int), "01014" .= (2 :: Int), "22134" .= (1 :: Int)]]
      dl <- playable =<< preview manager db deck
      lookup "01014" dl.cards `shouldBe` Just 3
      dl.messengerOfTheKingAlly `shouldBe` Just "01014"

    it "adds a missing slots repetition without losing the chosen ally" do
      let deck = object ["heroes" .= object ["01012" .= (1 :: Int), "9901014" .= (1 :: Int)], "slots" .= object ["01012" .= (1 :: Int), "22134" .= (1 :: Int)]]
      result <- preview manager db deck
      dl <- playable result
      dl.cards `shouldBe` [("01014", 1)]
      -- The fallback includes the synthetic hero's summed printed stats.
      field @(Maybe Int) "startingThreat" (field @Value "deck" result) `shouldBe` Just 18

    it "rejects a synthetic hero without its contract" do
      result <- preview manager db $ object ["heroes" .= object ["9901014" .= (1 :: Int)]]
      field @(Maybe DeckList) "list" result `shouldBe` Nothing
      field @(Maybe Text) "problem" result `shouldSatisfy` isJust

    it "resolves all 84 published MotKA codes to playable underlying allies" do
      codes <- either error pure =<< eitherDecodeFileStrict' "test-decks/fixtures/motka-codes.json"
      length (codes :: [Text]) `shouldBe` 84
      for_ codes \code -> do
        let deck = object ["heroes" .= object ["01012" .= (1 :: Int), fromString (unpack code) .= (1 :: Int)], "slots" .= object ["22134" .= (1 :: Int)]]
        dl <- playable =<< preview manager db deck
        dl.messengerOfTheKingAlly `shouldBe` messengerOfTheKingOriginal (CardCode code)
        sum (map snd dl.cards) `shouldBe` 1

    it "rejects invented aliases and neutral allies" do
      messengerOfTheKingOriginal "9901073" `shouldBe` Nothing
      messengerOfTheKingOriginal "9900000" `shouldBe` Nothing
      messengerOfTheKingOriginal "9901013" `shouldBe` Nothing

    it "rejects a restored fourth copy and duplicate intended hero titles" do
      let tooMany = deckList ["01012", "9901014"] [("01014", 3), ("22134", 1)]
          duplicate = deckList ["06081", "9901014"] [("22134", 1)]
      deckProblem tooMany `shouldSatisfy` isJust
      deckProblem duplicate `shouldSatisfy` isJust
      sharedHeroes [deckList ["06081"] [], deckList ["01012", "9901014"] [("22134", 1)]] `shouldBe` ["Faramir"]

    it "defaults the selection for pre-existing deck JSON" do
      let old = object ["heroes" .= (["01012"] :: [CardCode]), "cards" .= ([] :: [(CardCode, Int)])]
      (messengerOfTheKingAlly <$> (fromJSON old :: Aeson.Result DeckList)) `shouldBe` Aeson.Success Nothing

    it "does not accept a preferred ally outside the deck" do
      let dl = (deckList ["01012"] [("22134", 1)]) {messengerOfTheKingAlly = Just "01014"}
      deckProblem dl `shouldSatisfy` isJust

    it "preserves the selection in saved player JSON and defaults it in older games" do
      let dl = deckList ["01012", "9901014"] [("22134", 1)]
      (player, _) <- either (error . show) pure =<< loadPlayer "MotKA" dl
      playerMessengerOfTheKingAlly player `shouldBe` Just "01014"
      fromJSON (toJSON player) `shouldBe` Aeson.Success player
      let old = case toJSON player of
            Object fields -> Object (KeyMap.delete "messengerOfTheKingAlly" fields)
            value -> value
      (playerMessengerOfTheKingAlly <$> (fromJSON old :: Aeson.Result Player)) `shouldBe` Aeson.Success Nothing
