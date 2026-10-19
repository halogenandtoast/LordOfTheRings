{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Random (evalRand, mkStdGen, randomIO)
import System.Random.Internal (StdGen (..))
import System.Random.SplitMix (seedSMGen', unseedSMGen)
import Data.Aeson (Result (..))
import Data.Aeson.Diff qualified as Diff
import Data.Map.Strict qualified as Map
import Data.Char (isAlphaNum)
import Data.Text qualified as T
import Database.Persist.Postgresql
import LordOfTheRings.Card (allEncounterCards, allPlayerCards)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Decklist
import LordOfTheRings.Frontend (FrontendMessage)
import LordOfTheRings.Game
import LordOfTheRings.Game.Public
import LordOfTheRings.GameT
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Prelude hiding (Handler, delete)
import LordOfTheRings.Scenario (Scenario, lookupScenario)
import LordOfTheRings.Scenario.Types (scenarioName)
import Auth
import BugUpload (uploadBugExport)
import LordOfTheRings.I18n (ikey, ikeyWith, textVar)
import CardDb (CardDb, loadCardDb)
import Decks
import Model
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status400, status401, status409, status500)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Yesod.Core
import Yesod.Persist.Core
import Yesod.WebSockets

data App = App
  { pool :: ConnectionPool
  , jwtSecret :: Text
  , channels :: TVar (Map LotrGameId (TChan LByteString))
  , cardDb :: CardDb
  , manager :: Manager
  }

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /games GamesR GET POST
  /games/#LotrGameId GameR GET POST DELETE
  /games/#LotrGameId/undo UndoR POST
  /games/#LotrGameId/reset ResetR POST
  /games/#LotrGameId/debug DebugR POST
  /games/#LotrGameId/export ExportR GET
  /games/#LotrGameId/file-bug BugR POST
  /import ImportR POST
  /cards CardsR GET
  /decks DecksR GET POST
  /decks/#LotrDeckId DeckR GET DELETE
  /register RegisterR POST
  /authenticate AuthenticateR POST
  /whoami WhoAmIR GET
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
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

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
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

-- | The signed-in user, or 401.
requireUserId :: Handler UserId
requireUserId = do
  secret <- getsYesod (.jwtSecret)
  subject <- liftIO . fmap join . traverse (verifyToken secret) =<< lookupToken
  maybe (sendResponseStatus status401 (ikey "error.signInFirst")) pure $ toSqlKey <$> (readMaybe . unpack =<< subject)

-- | A game the signed-in user owns, or 404: other people's games do not exist for you.
requireOwnedGame :: LotrGameId -> Handler LotrGame
requireOwnedGame gid = do
  userId <- requireUserId
  row <- runDB $ get404 gid
  unless (row.lotrGameUserId == userId) notFound
  pure row

data UserView = UserView {id :: UserId, username :: Text, email :: Text}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Session = Session {token :: Text, user :: UserView}
  deriving stock (Generic)
  deriving anyclass (ToJSON)

newSession :: Entity User -> Handler Value
newSession (Entity userId user) = do
  secret <- getsYesod (.jwtSecret)
  token <- liftIO $ signToken secret (tshow $ fromSqlKey userId)
  pure $ toJSON $ Session token (UserView userId user.userUsername user.userEmail)

data Registration = Registration {username :: Text, email :: Text, password :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postRegisterR :: Handler Value
postRegisterR = do
  reg <- requireCheckJsonBody @_ @Registration
  let username = T.strip reg.username
      email = toLower (T.strip reg.email)
  when (null username || null email) $ sendResponseStatus status400 (ikey "error.registrationIncomplete")
  when (length reg.password < 8) $ sendResponseStatus status400 (ikey "error.passwordTooShort")
  taken <- runDB $ (||) <$> (isJust <$> getBy (UniqueUsername username)) <*> (isJust <$> getBy (UniqueEmail email))
  when taken $ sendResponseStatus status409 (ikey "error.alreadyRegistered")
  digest <- maybe (error "bcrypt failed") pure =<< liftIO (hashPassword reg.password)
  now <- liftIO getCurrentTime
  let user = User {userUsername = username, userEmail = email, userPasswordDigest = digest, userCreatedAt = now}
  userId <- runDB $ insert user
  newSession (Entity userId user)

data Credentials = Credentials {login :: Text, password :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Sign in with either a username or an email address.
postAuthenticateR :: Handler Value
postAuthenticateR = do
  creds <- requireCheckJsonBody @_ @Credentials
  let login = T.strip creds.login
  found <- runDB $ maybe (getBy $ UniqueEmail $ toLower login) (pure . Just) =<< getBy (UniqueUsername login)
  case found of
    Just user | checkPassword (entityVal user).userPasswordDigest creds.password -> newSession user
    _ -> sendResponseStatus status401 (ikey "error.badLogin")

getWhoAmIR :: Handler Value
getWhoAmIR = do
  userId <- requireUserId
  user <- runDB $ get404 userId
  pure $ toJSON $ UserView userId user.userUsername user.userEmail

-- | Every connected client of a game listens on that game's channel.
gameChannel :: LotrGameId -> Handler (TChan LByteString)
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

decodeOrFail :: (FromJSON a) => Text -> Value -> Handler a
decodeOrFail what v = case fromJSON v of
  Success a -> pure a
  Error err -> sendResponseStatus status400 ("could not read the stored " <> what <> ": " <> pack err)

-- | A random generator is stored as its splitmix seed, so a reloaded game rolls the same.
showGen :: StdGen -> Text
showGen = tshow . unseedSMGen . unStdGen

readGen :: Text -> Handler StdGen
readGen =
  maybe (sendResponseStatus status400 ("could not read the stored random generator" :: Text)) (pure . StdGen . seedSMGen')
    . readMaybe
    . unpack

-- | Load a stored game into a fresh environment whose log goes to the game's clients.
loadGame :: LotrGameId -> LotrGame -> Handler GameEnv
loadGame gid row = do
  g <- decodeOrFail "game" row.lotrGameCurrentData
  queue <- decodeOrFail "queue" row.lotrGameCurrentQueue
  gen <- readGen row.lotrGameRandomGen
  chan <- gameChannel gid
  liftIO $ envFor chan g queue gen

envFor :: TChan LByteString -> Game -> [Message] -> StdGen -> IO GameEnv
envFor chan g queue gen = do
  env <- newGameEnv g (logTo chan) 0
  runGameT env $ setQueue queue
  writeIORef (gameRandomGen env) gen
  pure env {gameDebug = 1}

logTo :: TChan LByteString -> FrontendMessage -> IO ()
logTo chan msg = atomically $ writeTChan chan $ encode $ toJSON msg

-- | The game, its queue and its random generator as they now stand.
currentState :: GameEnv -> IO (Game, [Message], StdGen)
currentState env = (,,) <$> runGameT env getGame <*> runGameT env peekQueue <*> readIORef (gameRandomGen env)

gameData :: LotrGame -> Game -> GameData
gameData row g = GameData True (Just $ publicGame g) (Just g.activePlayer) (row.lotrGameStep > 0)

broadcast :: LotrGameId -> LotrGame -> Game -> Handler ()
broadcast gid row g = do
  chan <- gameChannel gid
  atomically $ writeTChan chan $ encode $ object ["tag" .= String "Update", "contents" .= gameData row g]

summary :: Entity LotrGame -> Maybe GameSummary
summary (Entity gid row) = case fromJSON row.lotrGameCurrentData of
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
        , threat = sum [p.threat | p <- toList g.players]
        , outcome = g.outcome
        }

getGamesR :: Handler Value
getGamesR = do
  userId <- requireUserId
  games <- runDB $ selectList [LotrGameUserId ==. userId] [Desc LotrGameUpdatedAt]
  pure $ toJSON $ mapMaybe summary games

-- | A deck to deal: one of the core starter decks by sphere, or one of the user's decks.
data DeckChoice = StarterDeck Text | ImportedDeck LotrDeckId

instance FromJSON DeckChoice where
  parseJSON = withObject "DeckChoice" \o -> (StarterDeck <$> o .: "starter") <|> (ImportedDeck <$> o .: "deckId")

data NewGame = NewGame {name :: Text, seed :: Maybe Int, scenario :: Maybe Text, deck :: Maybe DeckChoice}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Deal a new game from a seed and play it up to its first question.
startGame :: Scenario -> DeckList -> Text -> Int -> IO (Game, [Message], StdGen)
startGame scenario deckList playerName seed = do
  (player, heroes) <- either (error . show) pure $ evalRand (loadPlayer playerName deckList) (mkStdGen seed)
  chan <- newBroadcastTChanIO
  env <- envFor chan (newGameIn scenario player heroes) [] (mkStdGen $ seed + 1)
  void $ runGameT env $ pushAll [Setup, BeginRound] >> mainLoop
  currentState env

postGamesR :: Handler Value
postGamesR = do
  userId <- requireUserId
  user <- runDB $ get404 userId
  new <- requireCheckJsonBody @_ @NewGame
  -- you play under your username
  let playerName = user.userUsername
      scenarioId = fromMaybe "passage-through-mirkwood" new.scenario
  scenario <- maybe (sendResponseStatus status400 (ikeyWith "error.noSuchScenario" [textVar "scenario" scenarioId])) pure $ lookupScenario (ScenarioId scenarioId)
  deckList <- case new.deck of
    Nothing -> pure Nothing
    Just (StarterDeck sphere) ->
      maybe (sendResponseStatus status400 (ikeyWith "error.noStarterDeck" [textVar "sphere" sphere])) (pure . Just) $ lookup sphere coreStarterDecks
    Just (ImportedDeck deckId) -> do
      db <- getsYesod (.cardDb)
      runDB (deckListFor db userId deckId) >>= either (sendResponseStatus status400) (pure . Just)
  (g, queue, gen) <- liftIO . startGame scenario (fromMaybe starterDeck deckList) playerName =<< maybe (liftIO randomIO) pure new.seed
  now <- liftIO getCurrentTime
  gid <-
    runDB
      $ insert
        LotrGame
          { lotrGameUserId = userId
          , lotrGameName = new.name
          , lotrGamePlayerName = playerName
          , lotrGameCurrentData = toJSON g
          , lotrGameCurrentQueue = toJSON queue
          , lotrGameRandomGen = showGen gen
          , lotrGameStep = 0
          , lotrGameScenario = scenarioId
          , lotrGameDeck = toJSON <$> deckList
          , lotrGameCreatedAt = now
          , lotrGameUpdatedAt = now
          }
  pure $ object ["id" .= gid]

handleSocket :: TChan LByteString -> WebSocketsT Handler ()
handleSocket chan = do
  readChannel <- atomically $ dupTChan chan
  forever $ atomically (readTChan readChannel) >>= sendTextData

getGameR :: LotrGameId -> Handler Value
getGameR gid = do
  row <- requireOwnedGame gid
  webSockets . handleSocket =<< gameChannel gid
  g <- decodeOrFail "game" row.lotrGameCurrentData
  pure $ toJSON $ gameData row g

data Answer = Answer
  { choice :: Int
  , player :: PlayerId
  , questionVersion :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGameR :: LotrGameId -> Handler ()
postGameR gid = do
  row <- requireOwnedGame gid
  answer <- requireCheckJsonBody @_ @Answer
  advance gid row \before -> do
    when (answer.questionVersion /= before.questionVersion) $
      sendResponseStatus status409 ("stale answer" :: Text)
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
  env <- loadGame gid row
  (before, queue, gen) <- liftIO $ currentState env
  play <- prepare before
  void $ liftIO $ runGameT env play
  (after, queue', gen') <- liftIO $ currentState env
  now <- liftIO getCurrentTime
  let step = row.lotrGameStep + 1
  row' <- runDB do
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
  broadcast gid row' after

deleteGameR :: LotrGameId -> Handler ()
deleteGameR gid = requireOwnedGame gid >> runDB (delete gid)

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
    Right parsed -> toJSON <$> runDB (saveDeck userId parsed)

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

data UndoTarget = LastAnswer | StartOfPhase | StartOfRound
  deriving stock (Eq)

-- | How many steps to take back: one answer, or every answer since the phase or round
-- began. With none taken yet this phase (or round), go back to the start of the last one.
stepsToUndo :: UndoTarget -> [LotrStep] -> Int
stepsToUndo target latestFirst = case (target, latestFirst) of
  (_, []) -> 0
  (LastAnswer, _) -> 1
  (StartOfPhase, newest : _) -> length $ takeWhile (samePhase newest) latestFirst
  (StartOfRound, newest : _) -> length $ takeWhile (sameRound newest) latestFirst
 where
  samePhase a b = sameRound a b && a.lotrStepPhase == b.lotrStepPhase
  sameRound a b = a.lotrStepRound == b.lotrStepRound

-- | Step a game back by applying the latest steps' patches, newest first.
postUndoR :: LotrGameId -> Handler ()
postUndoR gid = do
  row <- requireOwnedGame gid
  target <-
    lookupGetParam "to" <&> \case
      Just "phase" -> StartOfPhase
      Just "round" -> StartOfRound
      _ -> LastAnswer
  steps <- runDB $ selectList [LotrStepGameId ==. gid] [Desc LotrStepStep]
  let undone = take (stepsToUndo target (map entityVal steps)) steps
  for_ (lastMay undone) \(Entity _ oldest) -> do
    previous <- foldM undoStep row.lotrGameCurrentData undone
    g <- decodeOrFail "game" previous
    now <- liftIO getCurrentTime
    row' <- runDB do
      deleteWhere [LotrStepId <-. map entityKey undone]
      updateGet
        gid
        [ LotrGameCurrentData =. previous
        , LotrGameCurrentQueue =. oldest.lotrStepQueue
        , LotrGameRandomGen =. oldest.lotrStepRandomGen
        , LotrGameStep =. row.lotrGameStep - length undone
        , LotrGameUpdatedAt =. now
        ]
    broadcast gid row' g
 where
  undoStep current (Entity _ step) = do
    patch <- decodeOrFail "undo patch" step.lotrStepPatchDown
    case Diff.patch patch current of
      Success v -> pure v
      Error err -> sendResponseStatus status400 ("could not undo: " <> pack @Text err)

postResetR :: LotrGameId -> Handler ()
postResetR gid = do
  row <- requireOwnedGame gid
  scenario <- maybe (sendResponseStatus status400 ("No such scenario: " <> row.lotrGameScenario)) pure $ lookupScenario (ScenarioId row.lotrGameScenario)
  deckList <- maybe (pure starterDeck) (decodeOrFail "deck") row.lotrGameDeck
  (g, queue, gen) <- liftIO . startGame scenario deckList row.lotrGamePlayerName =<< liftIO randomIO
  now <- liftIO getCurrentTime
  row' <- runDB do
    deleteWhere [LotrStepGameId ==. gid]
    updateGet
      gid
      [ LotrGameCurrentData =. toJSON g
      , LotrGameCurrentQueue =. toJSON queue
      , LotrGameRandomGen =. showGen gen
      , LotrGameStep =. 0
      , LotrGameUpdatedAt =. now
      ]
  broadcast gid row' g

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
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data ExportedStep = ExportedStep
  { step :: Int
  , patchDown :: Value
  , queue :: Value
  , randomGen :: Text
  , round :: Int
  , phase :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

exportVersion :: Int
exportVersion = 1

-- | A game the signed-in user owns, as an export.
exportGame :: LotrGameId -> Handler (LotrGame, ExportedGame)
exportGame gid = do
  row <- requireOwnedGame gid
  steps <- runDB $ selectList [LotrStepGameId ==. gid] [Asc LotrStepStep]
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
            [ ExportedStep st.lotrStepStep st.lotrStepPatchDown st.lotrStepQueue st.lotrStepRandomGen st.lotrStepRound st.lotrStepPhase
            | Entity _ st <- steps
            ]
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
  unless (exported.version == exportVersion) $ sendResponseStatus status400 (ikey "error.unsupportedExport")
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
          , lotrGameCreatedAt = now
          , lotrGameUpdatedAt = now
          }
    insertMany_
      [ LotrStep gid st.step st.patchDown st.queue st.randomGen st.round st.phase now
      | st <- exported.steps
      ]
    pure gid
  pure $ object ["id" .= gid]

getCardsR :: Handler Value
getCardsR = pure $ toJSON $ allPlayerCards <> allEncounterCards

main :: IO ()
main = do
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
  database <- fromMaybe "host=localhost dbname=lordoftherings" <$> lookupEnv "DATABASE_URL"
  pool <- runStdoutLoggingT $ createPostgresqlPool (encodeUtf8 $ pack database) 10
  runSqlPool (runMigration migrateAll) pool
  channels <- newTVarIO mempty
  jwtSecret <- maybe "development-only-secret-set-JWT_SECRET-in-production" pack <$> lookupEnv "JWT_SECRET"
  -- HS256 signing needs a key of at least 256 bits
  when (length (encodeUtf8 jwtSecret) < 32) $ error "JWT_SECRET must be at least 32 bytes"
  cardsPath <- fromMaybe "frontend/public/data/cards.json" <$> lookupEnv "CARDS_JSON"
  cardDb <- loadCardDb cardsPath >>= either (\err -> error $ "could not load " <> cardsPath <> ": " <> err) pure
  manager <- newTlsManager
  warp port App {pool, jwtSecret, channels, cardDb, manager}
