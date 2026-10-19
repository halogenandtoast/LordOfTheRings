-- | The card browser's database (@cards.json@), read once at startup so decks can
-- be resolved against it. The cards are kept as the JSON they arrived as, because
-- the deck page renders them with the same code the browser does.
module CardDb (CardDb, loadCardDb, lookupCard, lookupRingsdbCode, cardName, cardRingsdbCode) where

import Data.Aeson.Types (Parser, parseMaybe)
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import LordOfTheRings.Prelude

data CardInfo = CardInfo
  { code :: Text
  , name :: Text
  , number :: Maybe Int
  , packCode :: Text
  , ringsdbCode :: Maybe Text
  }

data PackInfo = PackInfo {pack :: Text, order :: Maybe Int, cycleOrder :: Maybe Int}

data CardDb = CardDb
  { byCode :: Map Text (CardInfo, Value)
  , byRingsdb :: Map Text Text
  }

loadCardDb :: FilePath -> IO (Either String CardDb)
loadCardDb path = do
  decoded <- eitherDecodeFileStrict' path
  pure do
    (cards, packs) <- maybe (Left "cards.json has no cards and packs") Right . parseMaybe cardIndex =<< decoded
    let infos = [(info, card) | card <- cards, Just info <- [parseMaybe cardInfo card]]
    pure
      CardDb
        { byCode = Map.fromList [(info.code, (info, card)) | (info, card) <- infos]
        , byRingsdb = ringsdbIndex packs (map fst infos)
        }
 where
  cardIndex = withObject "index" \o -> (,) <$> o .: "cards" <*> (traverse packInfo =<< o .: "packs")
  packInfo = withObject "pack" \o -> PackInfo <$> o .: "code" <*> o .:? "order" <*> o .:? "cycleOrder"
  cardInfo :: Value -> Parser CardInfo
  cardInfo = withObject "card" \o ->
    CardInfo <$> o .: "code" <*> o .: "name" <*> o .:? "number" <*> o .: "packCode" <*> o .:? "ringsdbCode"

-- | RingsDB code to our card code.
--
-- A RingsDB code is not unique here: reprints in later products share the original's.
-- Decks are written against the original, so a collision prefers the card whose number
-- matches the code's last three digits, then the earliest product, then the shorter
-- code (the importer suffixes the second card at a number).
ringsdbIndex :: [PackInfo] -> [CardInfo] -> Map Text Text
ringsdbIndex packs cards =
  Map.mapMaybe pick $ Map.fromListWith (<>) [(r, [c]) | c <- cards, Just r <- [c.ringsdbCode]]
 where
  packOrder = Map.fromList [(p.pack, (p.cycleOrder, p.order)) | p <- packs]
  pick cs = (.code) <$> headMay (sortOn rank (preferNumbered cs))
  preferNumbered cs = case filter numberMatches cs of
    [] -> cs
    narrowed -> narrowed
  numberMatches c = maybe False ((== c.number) . ringsdbNumber) c.ringsdbCode
  rank c =
    ( fromMaybe maxBound (fst =<< Map.lookup c.packCode packOrder)
    , fromMaybe maxBound (snd =<< Map.lookup c.packCode packOrder)
    , T.length c.code
    , c.code
    )

-- | The last three digits of a RingsDB code are the card's number in its product.
ringsdbNumber :: Text -> Maybe Int
ringsdbNumber code = case unpack (T.takeEnd 3 code) of
  ds | length ds == 3, all isDigit ds -> readMay ds
  _ -> Nothing

lookupCard :: Text -> CardDb -> Maybe Value
lookupCard code db = snd <$> Map.lookup code db.byCode

lookupRingsdbCode :: Text -> CardDb -> Maybe Text
lookupRingsdbCode code db = Map.lookup code db.byRingsdb

cardRingsdbCode :: Text -> CardDb -> Maybe Text
cardRingsdbCode code db = (.ringsdbCode) . fst =<< Map.lookup code db.byCode

cardName :: Text -> CardDb -> Maybe Text
cardName code db = (.name) . fst <$> Map.lookup code db.byCode
