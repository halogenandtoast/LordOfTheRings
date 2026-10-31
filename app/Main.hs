{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Auth
import BugUpload (uploadBugExport)
import CardDb (CardDb, loadCardDb)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Random (evalRand, mkStdGen, randomIO)
import Data.Aeson (Result (..))
import Data.Aeson.Diff qualified as Diff
import Data.Char (isAlphaNum)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime, nominalDay)
import Database.Persist.Postgresql
import Decks
import GameLog
import LordOfTheRings.Campaign
import LordOfTheRings.Card (
  CardCode (..),
  CardDef (..),
  Display (..),
  Sphere,
  allEncounterCards,
  allPlayerCards,
  toCardCode,
  toCardDef,
 )
import LordOfTheRings.Classes.Entity (overAttrs, toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Decklist hiding (deckList)
import LordOfTheRings.Frontend (FrontendMessage (..))
import LordOfTheRings.Game
import LordOfTheRings.Game.Public
import LordOfTheRings.GameT
import LordOfTheRings.I18n (ikey, ikeyWith, textVar)
import LordOfTheRings.Id
import LordOfTheRings.Log (chat)
import LordOfTheRings.Log.Entry
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Prelude hiding (Handler, delete)
import LordOfTheRings.Scenario (Scenario, lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (scenarioCampaign), modeL, scenarioName)
import Model
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status400, status401, status409, status500)
import System.Environment (lookupEnv)
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen', unseedSMGen)
import Text.Read (readMaybe)
import Yesod.Core
import Yesod.Persist.Core
import Yesod.WebSockets

{- | Who plays: with friends, who sits in which seat (seat n playing the nth player);
otherwise the owner plays every seat.
-}
data Table = Table
  { withFriends :: Bool
  , seats :: Int
  , owner :: UserId
  , seated :: [SeatView]
  }
  deriving stock Generic
  deriving anyclass ToJSON

data SeatView = SeatView {position :: Int, userId :: UserId, username :: Text}
  deriving stock Generic
  deriving anyclass ToJSON

{- | What goes out to a game's clients: what the game said as it is, log entries to the
viewers who may read them (by the table and its player order), and the game, sent to
each client as its viewer may see it, with the log afresh when it was undone or dealt.
-}
data Outgoing
  = Said LByteString
  | Told Table [PlayerId] LogEntry
  | Updated LotrGame Table (Maybe Game) (Maybe [LogEntry]) Bool

data App = App
  { pool :: ConnectionPool
  , jwtSecret :: Text
  , channels :: TVar (Map LotrGameId (TChan Outgoing))
  , cardDb :: CardDb
  , manager :: Manager
  }

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /games GamesR GET POST
  /games/#LotrGameId GameR GET POST DELETE
  /games/#LotrGameId/spectate SpectateR GET
  /games/#LotrGameId/join JoinR POST
  /games/#LotrGameId/leave LeaveR POST
  /games/#LotrGameId/start StartR POST
  /games/#LotrGameId/undo UndoR POST
  /games/#LotrGameId/log LogR GET
  /games/#LotrGameId/chat ChatR POST
  /games/#LotrGameId/reset ResetR POST
  /games/#LotrGameId/debug DebugR POST
  /games/#LotrGameId/export ExportR GET
  /games/#LotrGameId/file-bug BugR POST
  /spectatable SpectatableR GET
  /admin AdminR GET
  /import ImportR POST
  /cards CardsR GET
  /decks DecksR GET POST
  /decks/#LotrDeckId DeckR GET DELETE
  /register RegisterR POST
  /authenticate AuthenticateR POST
  /whoami WhoAmIR GET
  /settings SettingsR PUT
  /campaigns CampaignsR POST
  /campaigns/#LotrCampaignId CampaignR GET DELETE
  /campaigns/#LotrCampaignId/next CampaignNextR POST
  /campaigns/#LotrCampaignId/xp CampaignXpR POST
|]

-- | The API authenticates with tokens, so there is no cookie session.
instance Yesod App where
  makeSessionBackend _ = pure Nothing

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getsYesod (.pool) >>= runSqlPool action

data GameData = GameData
  { started :: Bool
  , game :: Maybe Value
  , playerId :: Maybe PlayerId
  , canUndo :: Bool
  , table :: Table
  , log :: Maybe [LogEntry]
  -- ^ the log's latest entries, when the client should take them afresh
  , campaignId :: Maybe LotrCampaignId
  -- ^ the campaign the game is a scenario of
  }
  deriving stock Generic
  deriving anyclass ToJSON

-- | What the home page lists for each game.
data GameSummary = GameSummary
  { id :: LotrGameId
  , name :: Text
  , playerName :: Text
  , scenario :: Text
  , round :: Int
  , phase :: Phase
  , threat :: Int
  , outcome :: Maybe Outcome
  , victoryPoints :: Int
  , waiting :: Bool
  -- ^ a game with friends that has not been dealt, waiting for seats to fill
  , easyMode :: Bool
  , nightmare :: Bool
  , campaignId :: Maybe LotrCampaignId
  , seats :: [SeatSummary]
  -- ^ who plays, in turn order, and with which heroes; none until the game is dealt
  }
  deriving stock Generic
  deriving anyclass ToJSON

data SeatSummary = SeatSummary
  { name :: Text
  , threat :: Int
  , eliminated :: Bool
  , heroes :: [HeroSummary]
  }
  deriving stock Generic
  deriving anyclass ToJSON

data HeroSummary = HeroSummary {code :: CardCode, sphere :: Maybe Sphere}
  deriving stock Generic
  deriving anyclass ToJSON

-- | The signed-in user, if there is one.
lookupUserId :: Handler (Maybe UserId)
lookupUserId = do
  secret <- getsYesod (.jwtSecret)
  subject <- liftIO . fmap join . traverse (verifyToken secret) =<< lookupToken
  pure $ toSqlKey <$> (readMaybe . unpack =<< subject)

-- | The signed-in user, or 401.
requireUserId :: Handler UserId
requireUserId = maybe (sendResponseStatus status401 (ikey "error.signInFirst")) pure =<< lookupUserId

requireAdmin :: Handler ()
requireAdmin = do
  user <- runDB . get404 =<< requireUserId
  unless user.userAdmin $ permissionDenied "admins only"

-- | A game the signed-in user owns, or 404: other people's games do not exist for you.
requireOwnedGame :: LotrGameId -> Handler LotrGame
requireOwnedGame gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (row.lotrGameUserId == userId) notFound
  pure row

{- | A game you play: your own, or one you have a seat at. A game with friends still
waiting for players is open to anyone signed in, so its invite link shows the lobby.
-}
requirePlayableGame :: LotrGameId -> Handler LotrGame
requirePlayableGame gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  seat <- runDB $ selectFirst [LotrSeatGameId ==. gid, LotrSeatUserId ==. userId] []
  unless
    (row.lotrGameUserId == userId || isJust seat || (row.lotrGameWithFriends && awaitingPlayers row))
    notFound
  pure row

-- | A game with friends before it is dealt.
awaitingPlayers :: LotrGame -> Bool
awaitingPlayers row = row.lotrGameCurrentData == Null

-- | A game you may watch: one open to spectators, your own, or any game for an admin.
requireWatchableGame :: LotrGameId -> Handler LotrGame
requireWatchableGame gid = do
  row <- runDB $ get404 gid
  userId <- lookupUserId
  admin <- maybe (pure False) (fmap (maybe False (.userAdmin)) . runDB . get) userId
  seat <-
    maybe
      (pure Nothing)
      (\u -> runDB $ selectFirst [LotrSeatGameId ==. gid, LotrSeatUserId ==. u] [])
      userId
  unless
    (row.lotrGameAllowSpectators || userId == Just row.lotrGameUserId || isJust seat || admin)
    notFound
  pure row

data UserView = UserView
  { id :: UserId
  , username :: Text
  , email :: Text
  , admin :: Bool
  , beta :: Bool
  , allowSpectatorsByDefault :: Bool
  }
  deriving stock Generic
  deriving anyclass ToJSON

data Session = Session {token :: Text, user :: UserView}
  deriving stock Generic
  deriving anyclass ToJSON

newSession :: Entity User -> Handler Value
newSession (Entity userId user) = do
  secret <- getsYesod (.jwtSecret)
  token <- liftIO $ signToken secret (tshow $ fromSqlKey userId)
  pure $ toJSON $ Session token (userView userId user)

userView :: UserId -> User -> UserView
userView userId user =
  UserView
    userId
    user.userUsername
    user.userEmail
    user.userAdmin
    user.userBeta
    user.userAllowSpectatorsByDefault

data Registration = Registration {username :: Text, email :: Text, password :: Text}
  deriving stock Generic
  deriving anyclass FromJSON

postRegisterR :: Handler Value
postRegisterR = do
  reg <- requireCheckJsonBody @_ @Registration
  let username = T.strip reg.username
      email = toLower (T.strip reg.email)
  when (null username || null email)
    $ sendResponseStatus status400 (ikey "error.registrationIncomplete")
  when (length reg.password < 8) $ sendResponseStatus status400 (ikey "error.passwordTooShort")
  taken <-
    runDB
      $ (||)
      <$> (isJust <$> getBy (UniqueUsername username))
      <*> (isJust <$> getBy (UniqueEmail email))
  when taken $ sendResponseStatus status409 (ikey "error.alreadyRegistered")
  digest <- maybe (error "bcrypt failed") pure =<< liftIO (hashPassword reg.password)
  now <- liftIO getCurrentTime
  let user =
        User
          { userUsername = username
          , userEmail = email
          , userPasswordDigest = digest
          , userAdmin = False
          , userBeta = False
          , userAllowSpectatorsByDefault = False
          , userCreatedAt = now
          }
  userId <- runDB $ insert user
  newSession (Entity userId user)

data Credentials = Credentials {login :: Text, password :: Text}
  deriving stock Generic
  deriving anyclass FromJSON

-- | Sign in with either a username or an email address.
postAuthenticateR :: Handler Value
postAuthenticateR = do
  creds <- requireCheckJsonBody @_ @Credentials
  let login = T.strip creds.login
  found <-
    runDB $ maybe (getBy $ UniqueEmail $ toLower login) (pure . Just) =<< getBy (UniqueUsername login)
  case found of
    Just user | checkPassword (entityVal user).userPasswordDigest creds.password -> newSession user
    _ -> sendResponseStatus status401 (ikey "error.badLogin")

getWhoAmIR :: Handler Value
getWhoAmIR = do
  userId <- requireUserId
  user <- runDB $ get404 userId
  pure $ toJSON $ userView userId user

newtype Settings = Settings {allowSpectatorsByDefault :: Bool}
  deriving stock Generic
  deriving anyclass FromJSON

putSettingsR :: Handler Value
putSettingsR = do
  userId <- requireUserId
  settings <- requireCheckJsonBody @_ @Settings
  user <- runDB $ updateGet userId [UserAllowSpectatorsByDefault =. settings.allowSpectatorsByDefault]
  pure $ toJSON $ userView userId user

-- | Every connected client of a game listens on that game's channel.
gameChannel :: LotrGameId -> Handler (TChan Outgoing)
gameChannel gid = do
  app <- getYesod
  atomically do
    channels <- readTVar app.channels
    case Map.lookup gid channels of
      Just chan -> pure chan
      Nothing -> do
        chan <- newBroadcastTChan
        writeTVar app.channels (Map.insert gid chan channels)
        pure chan

decodeOrFail :: FromJSON a => Text -> Value -> Handler a
decodeOrFail what v = case fromJSON v of
  Success a -> pure a
  Error err -> sendResponseStatus status400 ("could not read the stored " <> what <> ": " <> pack err)

{- | The decks reset deals again: none for a lone starter deck, one deck as itself (as games
made before there were seats store it), or one per seat.
-}
storedDecks :: NonEmpty (Maybe DeckList) -> Maybe Value
storedDecks = \case
  Nothing :| [] -> Nothing
  Just deckList :| [] -> Just $ toJSON deckList
  deckLists -> Just $ toJSON $ fromMaybe starterDeck <$> deckLists

seatDecks :: Value -> Value
seatDecks = \case
  v@(Array _) -> v
  v -> toJSON [v]

-- | A random generator is stored as its splitmix seed, so a reloaded game rolls the same.
showGen :: StdGen -> Text
showGen = tshow . unseedSMGen . unStdGen

readGen :: Text -> Handler StdGen
readGen =
  maybe
    (sendResponseStatus status400 ("could not read the stored random generator" :: Text))
    (pure . StdGen . seedSMGen')
    . readMaybe
    . unpack

{- | Load a stored game into a fresh environment, with what it says kept to send once
the move is stored.
-}
loadGame :: LotrGame -> Handler (GameEnv, IORef [FrontendMessage])
loadGame row = do
  g <- decodeOrFail "game" row.lotrGameCurrentData
  queue <- decodeOrFail "queue" row.lotrGameCurrentQueue
  gen <- readGen row.lotrGameRandomGen
  liftIO $ envFor g queue gen

{- | What the game says is kept, newest first, rather than sent as it is said: its log
entries are numbered as they are stored, which has to wait for the move to be.
-}
envFor :: Game -> [Message] -> StdGen -> IO (GameEnv, IORef [FrontendMessage])
envFor g queue gen = do
  said <- newIORef []
  env <- newGameEnv g (\msg -> modifyIORef' said (msg :)) 0
  runGameT env $ setQueue queue
  writeIORef (gameRandomGen env) gen
  pure (env {gameDebug = 1}, said)

{- | What the game said, in order, to the game's clients: each log entry to whoever may
read it.
-}
publish :: LotrGameId -> LotrGame -> [PlayerId] -> [FrontendMessage] -> Handler ()
publish gid row order said = do
  chan <- gameChannel gid
  table <- tableFor gid row
  atomically $ for_ said $ writeTChan chan . \case
    Logged e -> Told table order e
    msg -> Said $ encode msg

-- | The game, its queue and its random generator as they now stand.
currentState :: GameEnv -> IO (Game, [Message], StdGen)
currentState env = (,,) <$> runGameT env getGame <*> runGameT env peekQueue <*> readIORef (gameRandomGen env)

-- | The game, nothing while it waits for players.
storedGame :: LotrGame -> Handler (Maybe Game)
storedGame row =
  if awaitingPlayers row then pure Nothing else Just <$> decodeOrFail "game" row.lotrGameCurrentData

tableFor :: LotrGameId -> LotrGame -> Handler Table
tableFor gid row = do
  seated <- runDB do
    seats <- selectList [LotrSeatGameId ==. gid] [Asc LotrSeatPosition]
    for seats \(Entity _ seat) ->
      SeatView seat.lotrSeatPosition seat.lotrSeatUserId
        . maybe "" (.userUsername)
        <$> get seat.lotrSeatUserId
  pure
    Table
      { withFriends = row.lotrGameWithFriends
      , seats = row.lotrGameSeats
      , owner = row.lotrGameUserId
      , seated
      }

{- | The game as a viewer may see it: with friends, only your own seat's hand, and only
the log entries meant for whoever sees it.
-}
gameData :: Maybe UserId -> LotrGame -> Table -> Maybe Game -> Maybe [LogEntry] -> GameData
gameData viewer row table g entries =
  GameData
    (isJust g)
    (toJSON . (\g' -> publicGameFor (seesHand table viewer g'.playerOrder) g') <$> g)
    ((.activePlayer) <$> g)
    (row.lotrGameStep > 0)
    table
    (filter (readsEntry $ seesHand table viewer (maybe [] (.playerOrder) g)) <$> entries)
    row.lotrGameCampaignId

-- | Whether a viewer sees a player's hand: with friends, only their own seat's.
seesHand :: Table -> Maybe UserId -> [PlayerId] -> PlayerId -> Bool
seesHand table viewer order pid
  | table.withFriends =
      isJust viewer
        && any (\seat -> Just seat.userId == viewer && order `atMay` seat.position == Just pid) table.seated
  | otherwise = True

updateFor :: Maybe UserId -> LotrGame -> Table -> Maybe Game -> Maybe [LogEntry] -> Bool -> LByteString
updateFor viewer row table g entries animate = encode $ object ["tag" .= String "Update", "contents" .= gameData viewer row table g entries, "animate" .= animate]

-- | The game to its clients, with the log afresh when given.
broadcast :: LotrGameId -> LotrGame -> Maybe Game -> Maybe [LogEntry] -> Handler ()
broadcast = broadcastAnimated True

-- | Undo restores the board without replaying card movements for any viewer.
broadcastAnimated :: Bool -> LotrGameId -> LotrGame -> Maybe Game -> Maybe [LogEntry] -> Handler ()
broadcastAnimated animate gid row g entries = do
  chan <- gameChannel gid
  table <- tableFor gid row
  atomically $ writeTChan chan $ Updated row table g entries animate

{- | The games summed up, with the decks dealt at each game with friends (null for the
starter deck), in seat order, read at once for all of them.
-}
summaries :: MonadIO m => [Entity LotrGame] -> ReaderT SqlBackend m [GameSummary]
summaries games = do
  seats <- selectList [LotrSeatGameId <-. [gid | Entity gid row <- games, row.lotrGameWithFriends]] [Asc LotrSeatPosition]
  let decks = Map.fromListWith (flip (<>)) [(seat.lotrSeatGameId, [seat.lotrSeatDeck]) | Entity _ seat <- seats]
  pure $ mapMaybe (\game -> summary (Map.findWithDefault [] (entityKey game) decks) game) games

{- | The heroes of the decks a game was dealt, a seat each, as reset deals them again.
An imported game keeps no deck, so it reads as the starter deck.
-}
dealtHeroes :: LotrGame -> [Maybe Value] -> [[CardCode]]
dealtHeroes row friendDecks = case decks of
  Success deckLists -> map (.heroes) deckLists
  Error _ -> []
 where
  decks
    | row.lotrGameWithFriends = traverse (maybe (pure starterDeck) fromJSON) friendDecks
    | otherwise = maybe (pure [starterDeck]) (fromJSON . seatDecks) row.lotrGameDeck

summary :: [Maybe Value] -> Entity LotrGame -> Maybe GameSummary
summary _ (Entity gid row) | awaitingPlayers row = do
  scenario <- lookupScenario (ScenarioId row.lotrGameScenario)
  pure
    GameSummary
      { id = gid
      , name = row.lotrGameName
      , playerName = row.lotrGamePlayerName
      , scenario = scenarioName (toAttrs scenario)
      , round = 0
      , phase = minBound
      , threat = 0
      , outcome = Nothing
      , victoryPoints = 0
      , waiting = True
      , easyMode = row.lotrGameEasyMode
      , nightmare = row.lotrGameNightmare
      , campaignId = row.lotrGameCampaignId
      , seats = []
      }
summary friendDecks (Entity gid row) = case fromJSON row.lotrGameCurrentData of
  Error _ -> Nothing
  Success (g :: Game) ->
    Just
      GameSummary
        { id = gid
        , name = row.lotrGameName
        , playerName = row.lotrGamePlayerName
        , scenario = scenarioName (toAttrs g.scenario)
        , round = g.round
        , phase = g.phase
        , threat = sum [p.threat | p <- toList (allPlayers g)]
        , outcome = g.outcome
        , victoryPoints = (toAttrs g.scenario).victoryPoints
        , waiting = False
        , easyMode = row.lotrGameEasyMode
        , nightmare = row.lotrGameNightmare
        , campaignId = row.lotrGameCampaignId
        , seats =
            [ SeatSummary
                p.name
                p.threat
                p.eliminated
                -- the heroes the deck began with, so a lost game still shows them; a
                -- game saved before they were kept shows those still in play, or, with
                -- none left, the deck it was dealt (an imported one's is not kept)
                ( case [HeroSummary (toCardCode h) (cdSphere $ toCardDef h) | h <- toList g.entities.heroes, h.owner == pid] of
                    stillInPlay
                      | notNull p.startingHeroes -> byCode p.startingHeroes
                      | notNull stillInPlay -> stillInPlay
                      | otherwise -> byCode dealt
                )
            | (pid, dealt) <- zip g.playerOrder (dealtHeroes row friendDecks <> repeat [])
            , Just p <- [Map.lookup pid (allPlayers g)]
            ]
        }
 where
  byCode codes = [HeroSummary c (cdSphere =<< Map.lookup c allPlayerCards) | c <- codes]

getGamesR :: Handler Value
getGamesR = do
  userId <- requireUserId
  games <- runDB do
    seatedAt <- map ((.lotrSeatGameId) . entityVal) <$> selectList [LotrSeatUserId ==. userId] []
    summaries =<< selectList ([LotrGameUserId ==. userId] ||. [LotrGameId <-. seatedAt]) [Desc LotrGameUpdatedAt]
  pure $ toJSON games

{- | A deck to deal: one of the core starter decks by sphere, or one of the user's decks.
| A starter, one of your decks, or a deck loaded for this game alone and not kept.
-}
data DeckSource = StarterDeck Text | ImportedDeck LotrDeckId | ListedDeck DeckList

-- | A deck and whether its cards follow the FAQ's errata (they do unless turned off).
data DeckChoice = DeckChoice DeckSource Bool

instance FromJSON DeckChoice where
  parseJSON = withObject "DeckChoice" \o -> do
    source <-
      (StarterDeck <$> o .: "starter")
        <|> (ImportedDeck <$> o .: "deckId")
        <|> (ListedDeck <$> o .: "list")
    DeckChoice source <$> o .:? "errata" .!= True

data NewGame = NewGame
  { name :: Text
  , seed :: Maybe Int
  , scenario :: Maybe Text
  , deck :: Maybe DeckChoice
  , decks :: Maybe [Maybe DeckChoice]
  {- ^ one deck per seat you take, Nothing being the starter deck; overrides deck. None
  leaves every seat open, to take in the lobby
  -}
  , seats :: Maybe Int
  {- ^ how many play, at least one per deck; you take the first seats and the rest are
  open, the game waiting for them
  -}
  , allowSpectators :: Maybe Bool
  , easyMode :: Maybe Bool
  , nightmare :: Maybe Bool
  -- ^ only for a scenario with a Nightmare Deck, and never with easy mode
  }
  deriving stock Generic
  deriving anyclass FromJSON

newtype JoinGame = JoinGame {deck :: Maybe DeckChoice}
  deriving stock Generic
  deriving anyclass FromJSON

-- | The deck chosen, Nothing for the starter deck.
resolveDeck :: UserId -> Maybe DeckChoice -> Handler (Maybe DeckList)
resolveDeck userId = \case
  Nothing -> pure Nothing
  Just (DeckChoice source errata) -> fmap (\dl -> dl {errata}) <$> resolveSource userId source

resolveSource :: UserId -> DeckSource -> Handler (Maybe DeckList)
resolveSource userId = \case
  StarterDeck sphere ->
    maybe
      (sendResponseStatus status400 (ikeyWith "error.noStarterDeck" [textVar "sphere" sphere]))
      (pure . Just)
      $ lookup sphere starterDecks
  ImportedDeck deckId -> do
    db <- getsYesod (.cardDb)
    deckList <- runDB (deckListFor db userId deckId) >>= either (sendResponseStatus status400) pure
    now <- liftIO getCurrentTime
    runDB $ update deckId [LotrDeckUsedAt =. Just now]
    pure $ Just deckList
  ListedDeck deckList -> either (sendResponseStatus status400) (pure . Just) (checkedDeckList deckList)

-- | Refuse decks that would put heroes with the same title at the table.
refuseSharedHeroes :: [DeckList] -> Handler ()
refuseSharedHeroes deckLists = case sharedHeroes deckLists of
  [] -> pure ()
  titles ->
    sendResponseStatus
      status400
      (ikeyWith "error.sharedHeroes" [textVar "heroes" (intercalate ", " titles)])

refuseBarredHeroes :: Text -> [DeckList] -> Handler ()
refuseBarredHeroes scenarioId deckLists = case barredHeroes scenarioId deckLists of
  [] -> pure ()
  titles ->
    sendResponseStatus
      status400
      (ikeyWith "error.barredHeroes" [textVar "heroes" (intercalate ", " titles)])

-- | Your seats in multi-handed solo, numbered when there are several.
soloSeats :: Text -> NonEmpty DeckList -> NonEmpty (Text, DeckList)
soloSeats name = \case
  deckList :| [] -> (name, deckList) :| []
  deckLists ->
    NE.zipWith
      (\n deckList -> (name <> " (" <> tshow n <> ")", deckList))
      (NE.iterate (+ 1) (1 :: Int))
      deckLists

{- | The seats of a game with friends, in order, with who sits there and their deck.
Someone sitting in several seats is numbered in each, as in multi-handed solo.
-}
friendSeats :: LotrGameId -> Handler [(Text, DeckList)]
friendSeats gid = do
  seats <- runDB $ selectList [LotrSeatGameId ==. gid] [Asc LotrSeatPosition]
  let users = map ((.lotrSeatUserId) . entityVal) seats
  for (zip [0 :: Int ..] seats) \(n, Entity _ seat) -> do
    name <- maybe "" (.userUsername) <$> runDB (get seat.lotrSeatUserId)
    let theirs = filter (== seat.lotrSeatUserId) users
        name' =
          if length theirs > 1
            then name <> " (" <> tshow (1 + length (filter (== seat.lotrSeatUserId) (take n users))) <> ")"
            else name
    (name',) <$> maybe (pure starterDeck) (decodeOrFail "deck") seat.lotrSeatDeck

{- | Deal a new game from a seed and play it up to its first question, with a seat for
each named deck, in easy mode when the flag says so.
-}
startGame
  :: Scenario
  -> GameMode
  -> NonEmpty (Text, DeckList)
  -> Int
  -> IO ((Game, [Message], StdGen), [FrontendMessage])
startGame scenario mode players seed = do
  seats <-
    either (error . show) pure
      . sequence
      $ evalRand (traverse (uncurry loadPlayer) players) (mkStdGen seed)
  (env, said) <-
    envFor (newTableIn (overAttrs (modeL .~ mode) scenario) seats) [] (mkStdGen $ seed + 1)
  void $ runGameT env $ pushAll [Setup, BeginRound] >> mainLoop
  (,) <$> currentState env <*> (reverse <$> readIORef said)

-- | The mode a game is dealt in, from its easy and nightmare flags.
gameMode :: Bool -> Bool -> GameMode
gameMode easy nightmare
  | nightmare = NightmareMode
  | easy = EasyMode
  | otherwise = NormalMode

postGamesR :: Handler Value
postGamesR = do
  userId <- requireUserId
  user <- runDB $ get404 userId
  new <- requireCheckJsonBody @_ @NewGame
  -- you play under your username
  let playerName = user.userUsername
      scenarioId = fromMaybe "passage-through-mirkwood" new.scenario
  scenario <-
    maybe
      (sendResponseStatus status400 (ikeyWith "error.noSuchScenario" [textVar "scenario" scenarioId]))
      pure
      $ lookupScenario (ScenarioId scenarioId)
  let choices = fromMaybe [new.deck] new.decks
  deckLists <- traverse (resolveDeck userId) choices
  refuseSharedHeroes $ fromMaybe starterDeck <$> deckLists
  refuseBarredHeroes scenarioId $ fromMaybe starterDeck <$> deckLists
  let easyMode = fromMaybe False new.easyMode
      nightmare = fromMaybe False new.nightmare
      seatCount = fromMaybe (length deckLists) new.seats
      -- seats left open are for friends, and the game waits for them
      friends = seatCount > length deckLists
  when (nightmare && (easyMode || isNothing (toAttrs scenario).nightmareDeck))
    $ sendResponseStatus status400 (ikey "error.noNightmareMode")
  when (seatCount < max 1 (length deckLists) || seatCount > 4)
    $ sendResponseStatus status400 (ikey "error.seatCount")
  now <- liftIO getCurrentTime
  let newGame' g queue gen =
        LotrGame
          { lotrGameUserId = userId
          , lotrGameName = new.name
          , lotrGamePlayerName = playerName
          , lotrGameCurrentData = g
          , lotrGameCurrentQueue = queue
          , lotrGameRandomGen = showGen gen
          , lotrGameStep = 0
          , lotrGameScenario = scenarioId
          , lotrGameDeck = storedDecks =<< nonEmpty deckLists
          , lotrGameAllowSpectators = fromMaybe False new.allowSpectators
          , lotrGameWithFriends = friends
          , lotrGameEasyMode = easyMode
          , lotrGameNightmare = nightmare
          , lotrGameCampaignId = Nothing
          , lotrGameSeats = seatCount
          , lotrGameCreatedAt = now
          , lotrGameUpdatedAt = now
          }
  gid <- case nonEmpty deckLists of
    Just dealt | not friends -> do
      ((g, queue, gen), said) <-
        liftIO
          . startGame scenario (gameMode easyMode nightmare) (soloSeats playerName $ fromMaybe starterDeck <$> dealt)
          =<< maybe (liftIO randomIO) pure new.seed
      runDB do
        gid <- insert $ newGame' (toJSON g) (toJSON queue) gen
        void $ storeLog gid (-1) now said
        pure gid
    _ ->
      -- dealt once the rest of the seats are taken
      runDB do
        gid <- insert $ newGame' Null (toJSON ([] :: [Message])) (mkStdGen 0)
        for_ (zip [0 ..] deckLists) \(position, deckList) -> insert_ $ LotrSeat gid position userId (toJSON <$> deckList)
        pure gid
  pure $ object ["id" .= gid]

{- | Take the first free seat at a game with friends, bringing a deck. The owner may take
several, anyone else one. Taking the last one deals the game.
-}
postJoinR :: LotrGameId -> Handler ()
postJoinR gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (row.lotrGameWithFriends && awaitingPlayers row)
    $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  joining <- requireCheckJsonBody @_ @JoinGame
  deckList <- resolveDeck userId joining.deck
  seated <- runDB $ selectList [LotrSeatGameId ==. gid] []
  when (row.lotrGameUserId /= userId && any ((== userId) . (.lotrSeatUserId) . entityVal) seated)
    $ sendResponseStatus status409 (ikey "error.alreadySeated")
  refuseSharedHeroes . (fromMaybe starterDeck deckList :) . map snd =<< friendSeats gid
  refuseBarredHeroes row.lotrGameScenario [fromMaybe starterDeck deckList]
  refuseFallenHeroes row [fromMaybe starterDeck deckList]
  case [0 .. row.lotrGameSeats - 1] \\ map ((.lotrSeatPosition) . entityVal) seated of
    [] -> sendResponseStatus status409 (ikey "error.noFreeSeat")
    (position : rest) -> do
      runDB $ insert_ $ LotrSeat gid position userId (toJSON <$> deckList)
      if null rest
        then deal gid row =<< maybe (error "no seats") pure . nonEmpty =<< friendSeats gid
        else broadcast gid row Nothing Nothing

-- | Give up your seats at a game with friends before it is dealt.
postLeaveR :: LotrGameId -> Handler ()
postLeaveR gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  runDB $ deleteWhere [LotrSeatGameId ==. gid, LotrSeatUserId ==. userId]
  broadcast gid row Nothing Nothing

{- | Deal a game with friends now, with the seats taken so far, closing the empty ones.
The seats are numbered again from 0 so they still match the players dealt.
-}
postStartR :: LotrGameId -> Handler ()
postStartR gid = do
  row <- requireOwnedGame gid
  unless (awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notWaitingForPlayers")
  seats <- runDB $ selectList [LotrSeatGameId ==. gid] [Asc LotrSeatPosition]
  when (null seats) $ sendResponseStatus status409 (ikey "error.noSeatTaken")
  row' <- runDB do
    -- out of the way first, as positions are unique
    for_ (zip [0 :: Int ..] seats) \(n, Entity sid _) -> update sid [LotrSeatPosition =. -1 - n]
    for_ (zip [0 :: Int ..] seats) \(n, Entity sid _) -> update sid [LotrSeatPosition =. n]
    updateGet gid [LotrGameSeats =. length seats]
  deal gid row' =<< maybe (error "no seats") pure . nonEmpty =<< friendSeats gid

-- | Deal the game again from the start, with a new seed.
deal :: LotrGameId -> LotrGame -> NonEmpty (Text, DeckList) -> Handler ()
deal gid row players' = do
  scenario' <-
    maybe (sendResponseStatus status400 ("No such scenario: " <> row.lotrGameScenario)) pure
      $ lookupScenario (ScenarioId row.lotrGameScenario)
  (scenario, players) <- inCampaign row scenario' players'
  ((g, queue, gen), said) <-
    liftIO . startGame scenario (gameMode row.lotrGameEasyMode row.lotrGameNightmare) players =<< liftIO randomIO
  now <- liftIO getCurrentTime
  (row', entries) <- runDB do
    deleteWhere [LotrStepGameId ==. gid]
    clearLog gid
    void $ storeLog gid (-1) now said
    (,)
      <$> updateGet
        gid
        [ LotrGameCurrentData =. toJSON g
        , LotrGameCurrentQueue =. toJSON queue
        , LotrGameRandomGen =. showGen gen
        , LotrGameStep =. 0
        , LotrGameUpdatedAt =. now
        ]
      <*> logTail gid
  broadcast gid row' (Just g) (Just entries)

handleSocket :: Maybe UserId -> TChan Outgoing -> WebSocketsT Handler ()
handleSocket viewer chan = do
  readChannel <- atomically $ dupTChan chan
  forever $ atomically (readTChan readChannel) >>= traverse_ sendTextData . \case
    Said bytes -> Just bytes
    Told table order e -> guard (readsEntry (seesHand table viewer order) e) $> encode (Logged e)
    Updated row table g entries animate -> Just $ updateFor viewer row table g entries animate

-- | The game as it stands, or its updates as they come over a websocket.
serveGame :: LotrGameId -> LotrGame -> Handler Value
serveGame gid row = do
  viewer <- lookupUserId
  webSockets . handleSocket viewer =<< gameChannel gid
  table <- tableFor gid row
  entries <- runDB $ logTail gid
  g <- storedGame row
  pure $ toJSON $ gameData viewer row table g (Just entries)

getGameR :: LotrGameId -> Handler Value
getGameR gid = serveGame gid =<< requirePlayableGame gid

{- | The same game and updates for someone watching. The socket only sends, and
answering needs the owner, so watching cannot change the game.
-}
getSpectateR :: LotrGameId -> Handler Value
getSpectateR gid = serveGame gid =<< requireWatchableGame gid

-- | Games in progress open to spectators, other than your own.
getSpectatableR :: Handler Value
getSpectatableR = do
  userId <- lookupUserId
  games <-
    runDB
      $ summaries
      =<< selectList
        ((LotrGameAllowSpectators ==. True) : maybe [] (\u -> [LotrGameUserId !=. u]) userId)
        [Desc LotrGameUpdatedAt, LimitTo 50]
  pure $ toJSON $ filter (isNothing . (.outcome)) games

data AdminDashboard = AdminDashboard
  { users :: Int
  , activeUsers :: Int
  , games :: Int
  , recentGames :: [GameSummary]
  }
  deriving stock Generic
  deriving anyclass ToJSON

{- | Counts of users and games, active meaning a game played in the last two weeks, and
the games played most recently.
-}
getAdminR :: Handler Value
getAdminR = do
  requireAdmin
  since <- addUTCTime (-14 * nominalDay) <$> liftIO getCurrentTime
  runDB do
    users <- Database.Persist.Postgresql.count ([] :: [Filter User])
    games <- Database.Persist.Postgresql.count ([] :: [Filter LotrGame])
    active <-
      rawSql "SELECT COUNT(DISTINCT user_id) FROM lotr_games WHERE updated_at >= ?" [toPersistValue since]
    recent <- summaries =<< selectList [] [Desc LotrGameUpdatedAt, LimitTo 20]
    pure
      $ toJSON
        AdminDashboard
          { users
          , activeUsers = sum (map unSingle active)
          , games
          , recentGames = recent
          }

data Answer = Answer
  { choice :: Int
  , player :: PlayerId
  , questionVersion :: Int
  }
  deriving stock Generic
  deriving anyclass FromJSON

postGameR :: LotrGameId -> Handler ()
postGameR gid = do
  row <- requirePlayableGame gid
  userId <- requireUserId
  seats <- runDB $ selectList [LotrSeatGameId ==. gid, LotrSeatUserId ==. userId] []
  answer <- requireCheckJsonBody @_ @Answer
  advance gid row \before -> do
    when (answer.questionVersion /= before.questionVersion)
      $ sendResponseStatus status409 ("stale answer" :: Text)
    -- only the seat asked can answer, and with friends only from its own player
    unless (Map.member answer.player before.question)
      $ sendResponseStatus status409 ("not asked" :: Text)
    let yours = mapMaybe ((before.playerOrder `atMay`) . (.lotrSeatPosition) . entityVal) seats
    unless
      (if row.lotrGameWithFriends then answer.player `elem` yours else row.lotrGameUserId == userId)
      notFound
    pure $ answerQuestion answer.player answer.choice

-- | Run any engine message against the game, for adjusting it by hand while testing.
postDebugR :: LotrGameId -> Handler ()
postDebugR gid = do
  row <- requireOwnedGame gid
  msg <- requireCheckJsonBody @_ @Message
  advance gid row \_ -> pure $ runDebug msg

-- | Play a stored game on from where it stands, keeping the move as an undoable step.
advance :: LotrGameId -> LotrGame -> (Game -> Handler (GameT Game)) -> Handler ()
advance gid row prepare = do
  (env, said) <- loadGame row
  (before, queue, gen) <- liftIO $ currentState env
  play <- prepare before
  void $ liftIO $ runGameT env play
  (after, queue', gen') <- liftIO $ currentState env
  now <- liftIO getCurrentTime
  let step = row.lotrGameStep + 1
  (row', stored) <- runDB do
    stored <- storeLog gid row.lotrGameStep now . reverse =<< readIORef said
    insert_
      LotrStep
        { lotrStepGameId = gid
        , lotrStepStep = step
        , lotrStepPatchDown = toJSON $ Diff.diff (toJSON after) (toJSON before)
        , lotrStepQueue = toJSON queue
        , lotrStepRandomGen = showGen gen
        , lotrStepRound = before.round
        , lotrStepPhase = tshow before.phase
        , lotrStepCreatedAt = now
        }
    updateGet
      gid
      [ LotrGameCurrentData =. toJSON after
      , LotrGameCurrentQueue =. toJSON queue'
      , LotrGameRandomGen =. showGen gen'
      , LotrGameStep =. step
      , LotrGameUpdatedAt =. now
      ]
      <&> (,stored)
  publish gid row' after.playerOrder stored
  broadcast gid row' (Just after) Nothing

deleteGameR :: LotrGameId -> Handler ()
deleteGameR gid = requireOwnedGame gid >> runDB (delete gid)

-- | The page of the log before the entry given, for scrolling back.
getLogR :: LotrGameId -> Handler Value
getLogR gid = do
  row <- requireWatchableGame gid
  viewer <- lookupUserId
  before <-
    maybe (sendResponseStatus status400 ("before what?" :: Text)) pure
      . (readMaybe . unpack =<<)
      =<< lookupGetParam "before"
  table <- tableFor gid row
  g <- storedGame row
  entries <- runDB $ logBefore gid before
  pure $ toJSON $ filter (readsEntry $ seesHand table viewer (maybe [] (.playerOrder) g)) entries

newtype Say = Say {text :: Text}
  deriving stock Generic
  deriving anyclass FromJSON

-- | Something a player says, into the log for the table to read.
postChatR :: LotrGameId -> Handler ()
postChatR gid = do
  row <- requirePlayableGame gid
  user <- runDB . get404 =<< requireUserId
  said <- T.strip . (.text) <$> requireCheckJsonBody @_ @Say
  when (null said || length said > 500)
    $ sendResponseStatus status400 ("say something shorter" :: Text)
  now <- liftIO getCurrentTime
  stored <-
    runDB
      $ storeLog
        gid
        row.lotrGameStep
        now
        [Logged $ chat [LogText user.userUsername, LogText ": ", LogText said]]
  g <- storedGame row
  publish gid row (maybe [] (.playerOrder) g) stored

getDecksR :: Handler Value
getDecksR = do
  userId <- requireUserId
  db <- getsYesod (.cardDb)
  toJSON <$> runDB (listDecks db userId)

-- | Import a deck from a RingsDB link or a deck file's contents.
postDecksR :: Handler Value
postDecksR = do
  userId <- requireUserId
  request <- requireCheckJsonBody @_ @ImportRequest
  App {cardDb, manager} <- getYesod
  liftIO (importDeck manager cardDb request) >>= \case
    Left err -> sendResponseStatus status400 (describeImportError err)
    Right parsed
      | savesDeck request -> toJSON <$> runDB (saveDeck userId parsed)
      | otherwise -> pure $ toJSON $ previewDeck cardDb parsed

getDeckR :: LotrDeckId -> Handler Value
getDeckR deckId = do
  userId <- requireUserId
  db <- getsYesod (.cardDb)
  maybe notFound (pure . toJSON) =<< runDB (loadDeck db userId deckId)

deleteDeckR :: LotrDeckId -> Handler ()
deleteDeckR deckId = do
  userId <- requireUserId
  deck <- runDB $ get deckId
  unless (((.lotrDeckUserId) <$> deck) == Just userId) notFound
  runDB $ delete deckId

data UndoTarget = LastAnswer | StartOfPhase | StartOfRound | ToStep Int
  deriving stock Eq

{- | How many steps to take back: one answer, every answer since the phase or round
began, or every answer since the step given, as a log entry offers. With none taken yet this phase (or round), go back to the start of the last one.
-}
stepsToUndo :: UndoTarget -> [LotrStep] -> Int
stepsToUndo target latestFirst = case (target, latestFirst) of
  (_, []) -> 0
  (LastAnswer, _) -> 1
  (ToStep n, _) -> length $ takeWhile ((> n) . (.lotrStepStep)) latestFirst
  (StartOfPhase, newest : _) -> length $ takeWhile (samePhase newest) latestFirst
  (StartOfRound, newest : _) -> length $ takeWhile (sameRound newest) latestFirst
 where
  samePhase a b = sameRound a b && a.lotrStepPhase == b.lotrStepPhase
  sameRound a b = a.lotrStepRound == b.lotrStepRound

-- | Step a game back by applying the latest steps' patches, newest first.
postUndoR :: LotrGameId -> Handler ()
postUndoR gid = do
  row <- requirePlayableGame gid
  stepParam <- (readMaybe . unpack =<<) <$> lookupGetParam "step"
  target <-
    lookupGetParam "to" <&> \case
      Just "phase" -> StartOfPhase
      Just "round" -> StartOfRound
      Just "step" | Just n <- stepParam -> ToStep n
      _ -> LastAnswer
  steps <- runDB $ selectList [LotrStepGameId ==. gid] [Desc LotrStepStep]
  let undone = take (stepsToUndo target (map entityVal steps)) steps
  for_ (lastMay undone) \(Entity _ oldest) -> do
    previous <- foldM undoStep row.lotrGameCurrentData undone
    g <- decodeOrFail "game" previous
    now <- liftIO getCurrentTime
    let step = row.lotrGameStep - length undone
    (row', entries) <- runDB do
      deleteWhere [LotrStepId <-. map entityKey undone]
      trimLog gid step
      (,)
        <$> updateGet
          gid
          [ LotrGameCurrentData =. previous
          , LotrGameCurrentQueue =. oldest.lotrStepQueue
          , LotrGameRandomGen =. oldest.lotrStepRandomGen
          , LotrGameStep =. step
          , LotrGameUpdatedAt =. now
          ]
        <*> logTail gid
    broadcastAnimated False gid row' (Just g) (Just entries)
 where
  undoStep current (Entity _ step) = do
    patch <- decodeOrFail "undo patch" step.lotrStepPatchDown
    case Diff.patch patch current of
      Success v -> pure v
      Error err -> sendResponseStatus status400 ("could not undo: " <> pack @Text err)

postResetR :: LotrGameId -> Handler ()
postResetR gid = do
  row <- requireOwnedGame gid
  when (awaitingPlayers row) $ sendResponseStatus status409 (ikey "error.notDealtYet")
  players <-
    if row.lotrGameWithFriends
      then maybe (error "no seats") pure . nonEmpty =<< friendSeats gid
      else
        soloSeats row.lotrGamePlayerName
          <$> maybe (pure $ starterDeck :| []) (decodeOrFail "deck" . seatDecks) row.lotrGameDeck
  deal gid row players

-- | Everything needed to carry a game elsewhere, undo history included.
data ExportedGame = ExportedGame
  { version :: Int
  , name :: Text
  , playerName :: Text
  , game :: Value
  , queue :: Value
  , randomGen :: Text
  , step :: Int
  , steps :: [ExportedStep]
  , log :: Maybe [LogEntry]
  -- ^ every entry, oldest first; missing from exports made before the log was stored
  }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

data ExportedStep = ExportedStep
  { step :: Int
  , patchDown :: Value
  , queue :: Value
  , randomGen :: Text
  , round :: Int
  , phase :: Text
  }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

exportVersion :: Int
exportVersion = 1

-- | A game the signed-in user plays, as an export.
exportGame :: LotrGameId -> Handler (LotrGame, ExportedGame)
exportGame gid = do
  row <- requirePlayableGame gid
  (steps, entries) <-
    runDB $ (,) <$> selectList [LotrStepGameId ==. gid] [Asc LotrStepStep] <*> wholeLog gid
  pure
    ( row
    , ExportedGame
        { version = exportVersion
        , name = row.lotrGameName
        , playerName = row.lotrGamePlayerName
        , game = row.lotrGameCurrentData
        , queue = row.lotrGameCurrentQueue
        , randomGen = row.lotrGameRandomGen
        , step = row.lotrGameStep
        , steps =
            [ ExportedStep
                st.lotrStepStep
                st.lotrStepPatchDown
                st.lotrStepQueue
                st.lotrStepRandomGen
                st.lotrStepRound
                st.lotrStepPhase
            | Entity _ st <- steps
            ]
        , log = Just entries
        }
    )

getExportR :: LotrGameId -> Handler Value
getExportR gid = do
  (row, exported) <- exportGame gid
  addHeader "Content-Disposition" $ "attachment; filename=\"" <> slug row.lotrGameName <> ".json\""
  pure $ toJSON exported
 where
  slug = T.intercalate "-" . T.words . T.filter (\c -> c == ' ' || c == '-' || isAlphaNum c) . toLower

-- | Upload the game's export for a bug report, giving back the URL to put in the issue.
postBugR :: LotrGameId -> Handler Value
postBugR gid = do
  (_, exported) <- exportGame gid
  manager <- getsYesod (.manager)
  liftIO (uploadBugExport manager (encode exported)) >>= \case
    Left err -> sendResponseStatus status500 err
    Right url -> pure $ object ["url" .= url]

-- | Load an exported game as a new game of the signed-in user's.
postImportR :: Handler Value
postImportR = do
  userId <- requireUserId
  exported <- requireCheckJsonBody @_ @ExportedGame
  unless (exported.version == exportVersion)
    $ sendResponseStatus status400 (ikey "error.unsupportedExport")
  -- refuse a file the engine cannot read before storing any of it
  imported <- decodeOrFail @Game "game" exported.game
  _ <- decodeOrFail @[Message] "queue" exported.queue
  _ <- readGen exported.randomGen
  now <- liftIO getCurrentTime
  gid <- runDB do
    gid <-
      insert
        LotrGame
          { lotrGameUserId = userId
          , lotrGameName = exported.name
          , lotrGamePlayerName = exported.playerName
          , lotrGameCurrentData = exported.game
          , lotrGameCurrentQueue = exported.queue
          , lotrGameRandomGen = exported.randomGen
          , lotrGameStep = exported.step
          , lotrGameScenario = unScenarioId $ toId imported.scenario
          , lotrGameDeck = Nothing
          , lotrGameAllowSpectators = False
          , lotrGameWithFriends = False
          , lotrGameEasyMode = (toAttrs imported.scenario).easyMode
          , lotrGameNightmare = (toAttrs imported.scenario).nightmare
          , lotrGameCampaignId = Nothing
          , lotrGameSeats = length imported.playerOrder
          , lotrGameCreatedAt = now
          , lotrGameUpdatedAt = now
          }
    insertMany_
      [ LotrStep gid st.step st.patchDown st.queue st.randomGen st.round st.phase now
      | st <- exported.steps
      ]
    restoreLog gid now (fromMaybe [] exported.log)
    pure gid
  pure $ object ["id" .= gid]

getCardsR :: Handler Value
getCardsR = pure $ toJSON $ allPlayerCards <> allEncounterCards

{- | Games were keyed by bigint, which the automatic migration cannot turn into a
uuid. Give each game a uuid and repoint its steps; the migration then puts back the
foreign key and unique constraint that went with the old column.
-}
gameIdsToUuids :: Text
gameIdsToUuids =
  T.unlines
    [ "DO $$ BEGIN"
    , "  IF (SELECT data_type FROM information_schema.columns"
    , "      WHERE table_name = 'lotr_games' AND column_name = 'id') = 'bigint' THEN"
    , "    ALTER TABLE lotr_games ADD COLUMN uuid uuid NOT NULL DEFAULT gen_random_uuid();"
    , "    ALTER TABLE lotr_steps ADD COLUMN game_uuid uuid;"
    , "    UPDATE lotr_steps s SET game_uuid = g.uuid FROM lotr_games g WHERE s.game_id = g.id;"
    , "    ALTER TABLE lotr_steps DROP COLUMN game_id;"
    , "    ALTER TABLE lotr_steps RENAME COLUMN game_uuid TO game_id;"
    , "    ALTER TABLE lotr_steps ALTER COLUMN game_id SET NOT NULL;"
    , "    ALTER TABLE lotr_games DROP COLUMN id;"
    , "    ALTER TABLE lotr_games RENAME COLUMN uuid TO id;"
    , "    ALTER TABLE lotr_games ADD PRIMARY KEY (id);"
    , "  END IF;"
    , "END $$;"
    ]

-- Campaign mode: a campaign is a row of its own holding the campaign log, and each of its
-- scenarios a game with friends of its own, dealt in the lobby as any other. Dealing one
-- hands it the log ('inCampaign'); once it is won the game holds the log as resolved, and
-- the next scenario takes that log back here.

{- | In campaign mode, the scenario takes the campaign log, the seats' heroes checked
against it and recorded (a fallen hero refused, a changed one adding to the threat
penalty), and each seat's deck takes its boons and burdens from the Campaign Pool.
-}
inCampaign :: LotrGame -> Scenario -> NonEmpty (Text, DeckList) -> Handler (Scenario, NonEmpty (Text, DeckList))
inCampaign row scenario players = case row.lotrGameCampaignId of
  Nothing -> pure (scenario, players)
  Just cid -> do
    l <- decodeOrFail @CampaignLog "campaign log" . (.lotrCampaignLog) =<< runDB (get404 cid)
    l' <- either refuseFallen pure $ seatHeroes [dl.heroes | (_, dl) <- toList players] l
    let withPool n (name, dl) = (name, dl {cards = seatDeck n l' dl.cards})
    pure (overAttrs (\a -> a {scenarioCampaign = Just l'}) scenario, NE.zipWith withPool (0 :| [1 ..]) players)

-- | Refuse a deck bringing a hero the campaign log lists as fallen.
refuseFallenHeroes :: LotrGame -> [DeckList] -> Handler ()
refuseFallenHeroes row deckLists = for_ row.lotrGameCampaignId \cid -> do
  l <- decodeOrFail @CampaignLog "campaign log" . (.lotrCampaignLog) =<< runDB (get404 cid)
  case filter (`elem` (l.fallenHeroes <> lockedHeroes l)) (concatMap (.heroes) deckLists) of
    [] -> pure ()
    fallen -> refuseFallen fallen

refuseFallen :: [CardCode] -> Handler a
refuseFallen codes =
  sendResponseStatus
    status400
    (ikeyWith "error.fallenHeroes" [textVar "heroes" (intercalate ", " [maybe (unCardCode c) (display . cdName) (Map.lookup c allPlayerCards) | c <- codes])])

data NewCampaign = NewCampaign
  { name :: Text
  , campaign :: CampaignId
  , seats :: Maybe Int
  , allowSpectators :: Maybe Bool
  , easyMode :: Maybe Bool
  , nightmare :: Maybe Bool
  }
  deriving stock Generic
  deriving anyclass FromJSON

-- | A scenario of the campaign, its game waiting in the lobby for its seats.
campaignGame :: UserId -> Text -> LotrCampaignId -> Text -> ScenarioId -> Int -> Bool -> Bool -> Bool -> UTCTime -> LotrGame
campaignGame userId playerName cid campaignName sid seatCount spectators easy nightmare now =
  LotrGame
    { lotrGameUserId = userId
    , lotrGameName = campaignName <> ": " <> maybe sid.unScenarioId (scenarioName . toAttrs) (lookupScenario sid)
    , lotrGamePlayerName = playerName
    , lotrGameCurrentData = Null
    , lotrGameCurrentQueue = toJSON ([] :: [Message])
    , lotrGameRandomGen = showGen (mkStdGen 0)
    , lotrGameStep = 0
    , lotrGameScenario = sid.unScenarioId
    , lotrGameDeck = Nothing
    , lotrGameAllowSpectators = spectators
    , lotrGameWithFriends = True
    , lotrGameSeats = seatCount
    , lotrGameEasyMode = easy
    , lotrGameNightmare = nightmare
    , lotrGameCampaignId = Just cid
    , lotrGameCreatedAt = now
    , lotrGameUpdatedAt = now
    }

-- | Start a campaign: its log, and its first scenario's game, waiting for its seats.
postCampaignsR :: Handler Value
postCampaignsR = do
  userId <- requireUserId
  user <- runDB $ get404 userId
  new <- requireCheckJsonBody @_ @NewCampaign
  let l = newCampaignLog new.campaign
      seatCount = fromMaybe 1 new.seats
  sid <- maybe (sendResponseStatus status400 (ikey "error.noSuchCampaign")) pure $ nextScenario l
  when (seatCount < 1 || seatCount > 4) $ sendResponseStatus status400 (ikey "error.seatCount")
  -- a campaign in nightmare mode plays each of its scenarios with its Nightmare Deck
  let easy = fromMaybe False new.easyMode
      nightmare = fromMaybe False new.nightmare
      scenarios = maybe [] (map fst . campaignDefScenarios) (lookupCampaign new.campaign)
  when (nightmare && (easy || any (maybe True (isNothing . (.nightmareDeck) . toAttrs) . lookupScenario) scenarios))
    $ sendResponseStatus status400 (ikey "error.noNightmareMode")
  now <- liftIO getCurrentTime
  (cid, gid) <- runDB do
    cid <- insert $ LotrCampaign userId new.name (toJSON l) now now
    gid <- insert $ campaignGame userId user.userUsername cid new.name sid seatCount (fromMaybe False new.allowSpectators) easy nightmare now
    pure (cid, gid)
  pure $ object ["id" .= gid, "campaignId" .= cid]

data CampaignScenarioView = CampaignScenarioView {id :: ScenarioId, name :: Text, card :: CardCode}
  deriving stock Generic
  deriving anyclass ToJSON

data CampaignGameView = CampaignGameView {id :: LotrGameId, scenario :: Text, outcome :: Maybe Outcome}
  deriving stock Generic
  deriving anyclass ToJSON

data CampaignView = CampaignView
  { id :: LotrCampaignId
  , name :: Text
  , campaignName :: Text
  , log :: CampaignLog
  -- ^ resolved by the scenario just won, if it was
  , scenarios :: [CampaignScenarioView]
  , games :: [CampaignGameView]
  -- ^ newest first
  , next :: Maybe ScenarioId
  , status :: Text
  -- ^ playing, won (the next scenario awaits), lost (play it again) or finished
  , upgrades :: [XpCard]
  -- ^ what XP may be spent on (the Dream-chaser campaign)
  , canSpendXp :: Bool
  -- ^ between scenarios: the next one waits in the lobby, not yet dealt
  }
  deriving stock Generic
  deriving anyclass ToJSON

{- | The campaign as it stands: its games, newest first, the last one's game, and the
log, as resolved if that game was won.
-}
campaignState :: LotrCampaignId -> LotrCampaign -> Handler ([Entity LotrGame], Maybe Game, CampaignLog)
campaignState cid campaign = do
  games <- runDB $ selectList [LotrGameCampaignId ==. Just cid] [Desc LotrGameCreatedAt]
  stored <- decodeOrFail "campaign log" campaign.lotrCampaignLog
  latest <- maybe (pure Nothing) (storedGame . entityVal) (headMay games)
  let resolved = do
        g <- latest
        guard (g.outcome == Just Won)
        scenarioCampaign (toAttrs g.scenario)
  pure (games, latest, fromMaybe stored resolved)

-- | The campaign's owner, or someone seated at one of its games.
requireCampaign :: LotrCampaignId -> Handler LotrCampaign
requireCampaign cid = do
  userId <- requireUserId
  campaign <- runDB $ get404 cid
  games <- runDB $ selectKeysList [LotrGameCampaignId ==. Just cid] []
  seat <- runDB $ selectFirst [LotrSeatUserId ==. userId, LotrSeatGameId <-. games] []
  unless (campaign.lotrCampaignUserId == userId || isJust seat) notFound
  pure campaign

getCampaignR :: LotrCampaignId -> Handler Value
getCampaignR cid = do
  campaign <- requireCampaign cid
  (games, latest, l) <- campaignState cid campaign
  def <- maybe (sendResponseStatus status400 (ikey "error.noSuchCampaign")) pure $ lookupCampaign l.campaign
  let next = nextScenario l
      outcomeOf row = case fromJSON @Game row.lotrGameCurrentData of
        Success g -> g.outcome
        Error _ -> Nothing
      status :: Text
      status = case (.outcome) =<< latest of
        _ | isNothing next -> "finished"
        Just Won -> "won"
        Just Lost -> "lost"
        Nothing -> "playing"
  pure
    $ toJSON
      CampaignView
        { id = cid
        , name = campaign.lotrCampaignName
        , campaignName = campaignDefName def
        , log = l
        , scenarios =
            [ CampaignScenarioView sid (maybe sid.unScenarioId (scenarioName . toAttrs) (lookupScenario sid)) (cdCardCode card)
            | (sid, card) <- campaignDefScenarios def
            ]
        , games = [CampaignGameView gid row.lotrGameScenario (outcomeOf row) | Entity gid row <- games]
        , next
        , status
        , upgrades = xpCards l
        , canSpendXp = maybe False (awaitingPlayers . entityVal) (headMay games)
        }

deleteCampaignR :: LotrCampaignId -> Handler ()
deleteCampaignR cid = do
  userId <- requireUserId
  campaign <- runDB $ get404 cid
  unless (campaign.lotrCampaignUserId == userId) notFound
  runDB $ delete cid

{- | Go on once the last scenario has ended: won, its resolved log is the campaign's and
the next scenario is dealt; lost, the same scenario is played again (Revised Core
rulebook: "they must play it again and defeat it before they can advance"). Its game
waits in the lobby for its seats, as many as the last had.
-}
postCampaignNextR :: LotrCampaignId -> Handler Value
postCampaignNextR cid = do
  userId <- requireUserId
  campaign <- runDB $ get404 cid
  unless (campaign.lotrCampaignUserId == userId) notFound
  (games, latest, l) <- campaignState cid campaign
  row <- maybe (sendResponseStatus status409 (ikey "error.campaignInProgress")) (pure . entityVal) (headMay games)
  when (isNothing $ (.outcome) =<< latest) $ sendResponseStatus status409 (ikey "error.campaignInProgress")
  sid <- maybe (sendResponseStatus status409 (ikey "error.campaignOver")) pure $ nextScenario l
  user <- runDB $ get404 userId
  now <- liftIO getCurrentTime
  gid <- runDB do
    update cid [LotrCampaignLog =. toJSON l, LotrCampaignUpdatedAt =. now]
    insert $ campaignGame userId user.userUsername cid campaign.lotrCampaignName sid row.lotrGameSeats row.lotrGameAllowSpectators row.lotrGameEasyMode row.lotrGameNightmare now
  pure $ object ["id" .= gid]

newtype SpendXp = SpendXp {card :: CardCode}
  deriving stock Generic
  deriving anyclass FromJSON

{- | Spend 1 XP to mark one of a card's checkboxes in the campaign log (Dream-chaser
Campaign Rules: "The players can only spend XP before or after a scenario"): while the
next scenario waits in the lobby, before it is dealt with the log.
-}
postCampaignXpR :: LotrCampaignId -> Handler Value
postCampaignXpR cid = do
  userId <- requireUserId
  campaign <- runDB $ get404 cid
  unless (campaign.lotrCampaignUserId == userId) notFound
  games <- runDB $ selectList [LotrGameCampaignId ==. Just cid] [Desc LotrGameCreatedAt]
  unless (maybe False (awaitingPlayers . entityVal) (headMay games)) $ sendResponseStatus status409 (ikey "error.campaignInProgress")
  spend <- requireCheckJsonBody @_ @SpendXp
  l <- decodeOrFail @CampaignLog "campaign log" campaign.lotrCampaignLog
  l' <- maybe (sendResponseStatus status400 (ikey "error.cannotSpendXp")) pure $ spendXp spend.card l
  now <- liftIO getCurrentTime
  runDB $ update cid [LotrCampaignLog =. toJSON l', LotrCampaignUpdatedAt =. now]
  pure $ toJSON l'

main :: IO ()
main = do
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
  database <- fromMaybe "host=localhost dbname=lordoftherings" <$> lookupEnv "DATABASE_URL"
  pool <- runStdoutLoggingT $ createPostgresqlPool (encodeUtf8 $ pack database) 10
  runSqlPool (rawExecute gameIdsToUuids [] >> runMigration migrateAll) pool
  channels <- newTVarIO mempty
  jwtSecret <-
    maybe "development-only-secret-set-JWT_SECRET-in-production" pack <$> lookupEnv "JWT_SECRET"
  -- HS256 signing needs a key of at least 256 bits
  when (length (encodeUtf8 jwtSecret) < 32) $ error "JWT_SECRET must be at least 32 bytes"
  cardsPath <- fromMaybe "frontend/public/data/cards.json" <$> lookupEnv "CARDS_JSON"
  cardDb <-
    loadCardDb cardsPath >>= either (\err -> error $ "could not load " <> cardsPath <> ": " <> err) pure
  manager <- newTlsManager
  warp port App {pool, jwtSecret, channels, cardDb, manager}
