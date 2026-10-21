{-# LANGUAGE DuplicateRecordFields #-}

-- | Decks: importing somebody else's deck list, and keeping it.
--
-- A deck comes from a RingsDB link or the contents of a deck JSON file, in either of
-- the shapes RingsDB hands out. The link is the only thing the server ever fetches
-- for a user, so it is restricted to RingsDB by host: see 'ringsdbId'.
--
-- Slots are stored by our card code with the code the source used beside it. A card
-- the importer cannot place is kept with no card code, so nothing a user imported
-- disappears without saying so.
module Decks (
  ImportRequest,
  savesDeck,
  importDeck,
  previewDeck,
  checkedDeckList,
  describeImportError,
  saveDeck,
  listDecks,
  loadDeck,
  deckListFor,
  ringsdbId,
) where

import CardDb
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser)
import Data.Aeson qualified as Aeson
import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Database.Persist
import Database.Persist.Sql (SqlBackend, fromSqlKey)
import LordOfTheRings.Card (CardCode (..), allPlayerCards)
import LordOfTheRings.Decklist (DeckList (..))
import LordOfTheRings.I18n (ikey, ikeyWith, textVar)
import LordOfTheRings.Prelude
import Model
import Network.HTTP.Client (Manager, httpLbs, parseRequest, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)

-- | What the deck page posts: a RingsDB link or a deck file's contents, and
-- optionally a name to use instead of the source's. Saved unless @save@ is false, as
-- for a deck loaded only to play the game at hand.
data ImportRequest = ImportRequest {url :: Maybe Text, deck :: Maybe Value, name :: Maybe Text, save :: Maybe Bool}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

savesDeck :: ImportRequest -> Bool
savesDeck request = fromMaybe True request.save

data ImportError
  = NothingToImport
  | NotARingsdbUrl Text
  | FetchFailed Text
  | MalformedJson String
  | NoCards

describeImportError :: ImportError -> Text
describeImportError = \case
  NothingToImport -> ikey "error.noDeckSource"
  NotARingsdbUrl u -> ikeyWith "error.notRingsdbLink" [textVar "url" u]
  FetchFailed why -> ikeyWith "error.fetchFailed" [textVar "reason" why]
  MalformedJson why -> ikeyWith "error.malformedDeck" [textVar "reason" (pack why)]
  NoCards -> ikey "error.noCards"

-- | A RingsDB deck list. RingsDB repeats each hero inside @slots@, and sends an empty
-- array for an empty sideboard; both are undone here.
data RingsdbDeck = RingsdbDeck
  { ringsdbDeckId :: Maybe Int
  , name :: Text
  , description :: Text
  , startingThreat :: Maybe Int
  , heroes :: Map Text Int
  , slots :: Map Text Int
  , sideslots :: Map Text Int
  , url :: Maybe Text
  }

instance FromJSON RingsdbDeck where
  parseJSON = withObject "RingsdbDeck" \o -> do
    ringsdbDeckId <- o .:? "id"
    name <- o .:? "name" .!= "Imported deck"
    markdown <- o .:? "description_md"
    plain <- o .:? "description"
    startingThreat <- o .:? "starting_threat"
    heroes <- codeMap o "heroes"
    slots <- codeMap o "slots"
    sideslots <- codeMap o "sideslots"
    url <- o .:? "url"
    pure RingsdbDeck {description = fromMaybe "" (nonBlank markdown <|> nonBlank plain), ..}
   where
    nonBlank = \case
      Just t | not (T.null (T.strip t)) -> Just t
      _ -> Nothing

-- | A @{code: quantity}@ object, tolerating a missing key, an empty array and
-- quantities written as strings.
codeMap :: Aeson.Object -> Key.Key -> Parser (Map Text Int)
codeMap o key = case KeyMap.lookup key o of
  Nothing -> pure mempty
  Just Aeson.Null -> pure mempty
  Just (Aeson.Array _) -> pure mempty
  Just (Aeson.Object entries) ->
    Map.fromList . filter ((> 0) . snd)
      <$> traverse (\(k, v) -> (Key.toText k,) <$> quantity v) (KeyMap.toList entries)
  Just _ -> fail (show key <> " should be an object of card code to quantity")
 where
  quantity = \case
    Aeson.Number n -> pure (truncate n)
    Aeson.String s | Just n <- readMay (unpack s) -> pure n
    v -> fail ("card quantity should be a number, got " <> show v)

-- | One line of a deck list, as the deck page reads it.
data Slot = Slot {quantity :: Int, sourceCode :: Text, cardCode :: Maybe Text, card :: Maybe Value}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Deck = Deck
  { deckId :: Int64
  , name :: Text
  , description :: Text
  , startingThreat :: Maybe Int
  , source :: Text
  , sourceId :: Maybe Text
  , sourceUrl :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , heroes :: [Slot]
  , slots :: [Slot]
  , sideslots :: [Slot]
  }
  deriving stock (Generic)

instance ToJSON Deck where
  toJSON = genericToJSON idField

data DeckSummary = DeckSummary
  { deckId :: Int64
  , name :: Text
  , startingThreat :: Maybe Int
  , source :: Text
  , sourceUrl :: Maybe Text
  , heroNames :: [Text]
  , heroCodes :: [Text]
  , cardCount :: Int
  , unresolvedCount :: Int
  , updatedAt :: UTCTime
  }
  deriving stock (Generic)

instance ToJSON DeckSummary where
  toJSON = genericToJSON idField

idField :: Options
idField = defaultOptions {fieldLabelModifier = \case "deckId" -> "id"; field -> field}

-- | A deck list resolved against the card database, owned by nobody yet.
data ParsedDeck = ParsedDeck
  { name :: Text
  , description :: Text
  , startingThreat :: Maybe Int
  , source :: Text
  , sourceId :: Maybe Text
  , sourceUrl :: Maybe Text
  , heroes :: [Slot]
  , slots :: [Slot]
  , sideslots :: [Slot]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | A deck loaded but not kept: what it holds, and the list the engine would deal from
-- it, or why it cannot.
data DeckPreview = DeckPreview
  { deck :: ParsedDeck
  , heroCodes :: [Text]
  , cardCount :: Int
  , list :: Maybe DeckList
  , problem :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

previewDeck :: CardDb -> ParsedDeck -> DeckPreview
previewDeck db parsed =
  DeckPreview
    { deck = parsed
    , heroCodes = [c | s <- parsed.heroes, Just c <- [flip cardRingsdbCode db =<< s.cardCode]]
    , cardCount = sum [s.quantity | s <- parsed.slots]
    , list = either (const Nothing) Just dealt
    , problem = either Just (const Nothing) dealt
    }
 where
  dealt = listFrom db (map lineOf parsed.heroes) (map lineOf parsed.slots)
  lineOf s = (s.sourceCode, s.cardCode, s.quantity)

-- | A deck list a client sent back from a preview: only cards the engine knows, each
-- at least once.
checkedDeckList :: DeckList -> Either Text DeckList
checkedDeckList deckList
  | null unknown && all ((> 0) . snd) deckList.cards && not (null deckList.heroes) = Right deckList
  | otherwise = Left $ ikeyWith "error.unknownCards" [textVar "cards" (intercalate ", " unknown)]
 where
  unknown = [code | CardCode code <- deckList.heroes <> map fst deckList.cards, not (Map.member (CardCode code) allPlayerCards)]

-- | Heroes come out of the draw deck; a card we cannot place stays as an unresolved slot.
toParsedDeck :: CardDb -> Text -> Maybe Text -> RingsdbDeck -> ParsedDeck
toParsedDeck db source override deck =
  ParsedDeck
    { name = case T.strip <$> override of
        Just n | not (T.null n) -> n
        _ -> T.strip deck.name
    , description = deck.description
    , startingThreat = deck.startingThreat
    , source
    , sourceId = tshow <$> deck.ringsdbDeckId
    , sourceUrl = deck.url
    , heroes = slotsFrom deck.heroes
    , slots = slotsFrom (Map.difference deck.slots deck.heroes)
    , sideslots = slotsFrom deck.sideslots
    }
 where
  -- keyed by card code, which is printing order: the order a deck list reads in
  slotsFrom m = [slot db code (lookupRingsdbCode code db <|> ours code) quantity | (code, quantity) <- Map.toAscList m]
  -- a file written against our own database names our codes instead
  ours code = code <$ lookupCard code db

slot :: CardDb -> Text -> Maybe Text -> Int -> Slot
slot db sourceCode cardCode quantity = Slot {quantity, sourceCode, cardCode, card = flip lookupCard db =<< cardCode}

-- | RingsDB has two kinds of deck: a published decklist, public to all, and a deck
-- still in somebody's collection, readable only when its owner shares their decks.
data RingsdbDeckKind = Decklist | SharedDeck

-- | The kind of deck and its id in a RingsDB URL, or 'Nothing' for anything else. A
-- bare number is taken for a published decklist.
--
-- Only @ringsdb.com@ and @www.ringsdb.com@, and only a path naming a deck. The server
-- never follows the URL it was given: it takes an id out of it and builds its own, so
-- a link pointed elsewhere on the network goes nowhere.
ringsdbId :: Text -> Maybe (RingsdbDeckKind, Text)
ringsdbId raw
  | T.null stripped = Nothing
  | allDigits stripped = Just (Decklist, stripped)
  | not onRingsdb = Nothing
  | otherwise = case break (`elem` deckPaths) segments of
      (_, path : rest) -> (if path `elem` ["deck", "decks"] then SharedDeck else Decklist,) <$> firstNumber rest
      _ -> Nothing
 where
  stripped = T.strip raw
  withoutScheme = snd (T.breakOnEnd "://" stripped)
  (hostPort, afterHost) = T.break (== '/') withoutScheme
  host = T.toLower (T.takeWhile (/= ':') (snd (T.breakOnEnd "@" hostPort)))
  onRingsdb = host `elem` ["ringsdb.com", "www.ringsdb.com"]
  segments = T.splitOn "/" (T.dropWhile (== '/') (T.takeWhile (`notElem` ['?', '#']) afterHost))
  deckPaths = ["decklist", "deck", "decklists", "decks"]
  firstNumber = \case
    [] -> Nothing
    p : rest | allDigits p -> Just p | otherwise -> firstNumber rest
  allDigits t = not (T.null t) && T.all isDigit t

-- | Where a deck of each kind is read and where people look at it.
ringsdbApi, ringsdbPage :: RingsdbDeckKind -> Text -> Text
ringsdbApi kind ident = case kind of
  Decklist -> "https://ringsdb.com/api/public/decklist/" <> ident <> ".json"
  -- the endpoint DragnCards reads: it answers only for owners who share their decks
  SharedDeck -> "https://ringsdb.com/api/oauth2/deck/load/" <> ident
ringsdbPage kind ident = case kind of
  Decklist -> "https://ringsdb.com/decklist/view/" <> ident
  SharedDeck -> "https://ringsdb.com/deck/view/" <> ident

-- | Fetch a deck by kind and id. The URL is built here, never taken from the user.
fetchRingsdbDeck :: Manager -> RingsdbDeckKind -> Text -> IO (Either ImportError Value)
fetchRingsdbDeck manager kind ringsdbDeck = do
  attempt <- tryAny do
    request <- parseRequest (unpack (ringsdbApi kind ringsdbDeck))
    httpLbs request {requestHeaders = [("accept", "application/json"), ("user-agent", "lotr")]} manager
  pure case attempt of
    Left err -> Left (FetchFailed (pack (takeWhile (/= '\n') (show err))))
    Right response -> case statusCode (responseStatus response) of
      404 -> Left (FetchFailed ("RingsDB has no public decklist " <> ringsdbDeck))
      status
        | status >= 400 -> Left (FetchFailed ("RingsDB answered " <> tshow status))
        | otherwise -> case eitherDecode (responseBody response) of
            -- RingsDB answers an unknown path with an HTML page and a 200
            Left _ -> Left (FetchFailed ("RingsDB did not return a deck for " <> ringsdbDeck))
            -- the shared-deck endpoint refuses with a 200 and a reason
            Right (Object o)
              | Just (Bool False) <- KeyMap.lookup "success" o ->
                  Left (FetchFailed (fromMaybe "RingsDB refused that deck" (KeyMap.lookup "error" o >>= asText)))
            Right value -> Right value
 where
  asText = \case
    String t -> Just t
    _ -> Nothing

-- | The whole import: fetch if asked to, parse, resolve.
importDeck :: Manager -> CardDb -> ImportRequest -> IO (Either ImportError ParsedDeck)
importDeck manager db request = case (T.strip <$> request.url, request.deck) of
  (Just u, _) | not (T.null u) -> case ringsdbId u of
    Nothing -> pure (Left (NotARingsdbUrl u))
    Just (kind, ringsdbDeck) ->
      fmap (finish "ringsdb" (Just (ringsdbPage kind ringsdbDeck)))
        <$> fetchRingsdbDeck manager kind ringsdbDeck
        <&> join
  (_, Just value) -> pure (finish "json" Nothing value)
  _ -> pure (Left NothingToImport)
 where
  finish source fallback value = case fromJSON value of
    Aeson.Error why -> Left (MalformedJson why)
    Aeson.Success deck -> do
      let located = withFallbackUrl fallback (toParsedDeck db source request.name deck)
      if null located.heroes && null located.slots then Left NoCards else Right located
  -- a deck fetched by link knows where it came from even when the payload does not say
  withFallbackUrl fallback ParsedDeck {..} = ParsedDeck {sourceUrl = sourceUrl <|> fallback, ..}

-- | Write a parsed deck to a user's collection and return it as stored.
saveDeck :: (MonadIO m) => UserId -> ParsedDeck -> ReaderT SqlBackend m Deck
saveDeck userId parsed = do
  now <- liftIO getCurrentTime
  key <-
    insert
      LotrDeck
        { lotrDeckUserId = userId
        , lotrDeckName = parsed.name
        , lotrDeckDescription = parsed.description
        , lotrDeckStartingThreat = parsed.startingThreat
        , lotrDeckSource = parsed.source
        , lotrDeckSourceId = parsed.sourceId
        , lotrDeckSourceUrl = parsed.sourceUrl
        , lotrDeckCreatedAt = now
        , lotrDeckUpdatedAt = now
        }
  insertMany_
    [ LotrDeckSlot key kind position s.sourceCode s.cardCode s.quantity
    | (kind, list) <- [("hero", parsed.heroes), ("draw", parsed.slots), ("side", parsed.sideslots)]
    , (position, s) <- zip [0 ..] list
    ]
  pure
    Deck
      { deckId = fromSqlKey key
      , name = parsed.name
      , description = parsed.description
      , startingThreat = parsed.startingThreat
      , source = parsed.source
      , sourceId = parsed.sourceId
      , sourceUrl = parsed.sourceUrl
      , createdAt = now
      , updatedAt = now
      , heroes = parsed.heroes
      , slots = parsed.slots
      , sideslots = parsed.sideslots
      }

-- | A user's decks, most recently touched first.
listDecks :: (MonadIO m) => CardDb -> UserId -> ReaderT SqlBackend m [DeckSummary]
listDecks db userId = do
  decks <- selectList [LotrDeckUserId ==. userId] [Desc LotrDeckUpdatedAt, Desc LotrDeckId]
  for decks \(Entity key d) -> do
    slots <- map entityVal <$> selectList [LotrDeckSlotDeckId ==. key] [Asc LotrDeckSlotPosition]
    pure
      DeckSummary
        { deckId = fromSqlKey key
        , name = d.lotrDeckName
        , startingThreat = d.lotrDeckStartingThreat
        , source = d.lotrDeckSource
        , sourceUrl = d.lotrDeckSourceUrl
        , heroNames = [n | s <- slots, s.lotrDeckSlotKind == "hero", Just n <- [flip cardName db =<< s.lotrDeckSlotCardCode]]
        , -- the RingsDB code, as the art is named by
          heroCodes = [c | s <- slots, s.lotrDeckSlotKind == "hero", Just c <- [flip cardRingsdbCode db =<< s.lotrDeckSlotCardCode]]
        , cardCount = sum [s.lotrDeckSlotQuantity | s <- slots, s.lotrDeckSlotKind == "draw"]
        , unresolvedCount = length [() | s <- slots, isNothing s.lotrDeckSlotCardCode]
        , updatedAt = d.lotrDeckUpdatedAt
        }

-- | One deck with its cards, or 'Nothing' for a deck this user does not own.
loadDeck :: (MonadIO m) => CardDb -> UserId -> LotrDeckId -> ReaderT SqlBackend m (Maybe Deck)
loadDeck db userId key =
  get key >>= \case
    Just d | d.lotrDeckUserId == userId -> do
      rows <- map entityVal <$> selectList [LotrDeckSlotDeckId ==. key] [Asc LotrDeckSlotPosition]
      let section kind = [slot db r.lotrDeckSlotSourceCode r.lotrDeckSlotCardCode r.lotrDeckSlotQuantity | r <- rows, r.lotrDeckSlotKind == kind]
      pure
        $ Just
          Deck
            { deckId = fromSqlKey key
            , name = d.lotrDeckName
            , description = d.lotrDeckDescription
            , startingThreat = d.lotrDeckStartingThreat
            , source = d.lotrDeckSource
            , sourceId = d.lotrDeckSourceId
            , sourceUrl = d.lotrDeckSourceUrl
            , createdAt = d.lotrDeckCreatedAt
            , updatedAt = d.lotrDeckUpdatedAt
            , heroes = section "hero"
            , slots = section "draw"
            , sideslots = section "side"
            }
    _ -> pure Nothing

-- | A stored deck as the engine deals it: heroes and draw deck by RingsDB code, the
-- engine's own. Refused, with the cards named, when the engine does not know a card.
deckListFor :: (MonadIO m) => CardDb -> UserId -> LotrDeckId -> ReaderT SqlBackend m (Either Text DeckList)
deckListFor db userId key =
  get key >>= \case
    Just d | d.lotrDeckUserId == userId -> do
      rows <- map entityVal <$> selectList [LotrDeckSlotDeckId ==. key] [Asc LotrDeckSlotPosition]
      let section kind = [(r.lotrDeckSlotSourceCode, r.lotrDeckSlotCardCode, r.lotrDeckSlotQuantity) | r <- rows, r.lotrDeckSlotKind == kind]
      pure $ listFrom db (section "hero") (section "draw")
    _ -> pure $ Left $ ikey "error.noSuchDeck"

-- | Heroes and draw deck, each line by the code its source used, our code and how
-- many, as the engine deals them.
listFrom :: CardDb -> [(Text, Maybe Text, Int)] -> [(Text, Maybe Text, Int)] -> Either Text DeckList
listFrom db heroes draw
  | null unknown = Right $ DeckList (map (CardCode . code) heroes) [(CardCode (code l), n) | l@(_, _, n) <- draw]
  | otherwise = Left $ ikeyWith "error.unknownCards" [textVar "cards" (intercalate ", " unknown)]
 where
  code (source, ours, _) = fromMaybe source (flip cardRingsdbCode db =<< ours)
  unknown = [fromMaybe (code l) (flip cardName db =<< ours) | l@(_, ours, _) <- heroes <> draw, not (Map.member (CardCode $ code l) allPlayerCards)]
