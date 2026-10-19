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
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.Persist.Postgresql
import LordOfTheRings.Card (allEncounterCards, allPlayerCards)
import LordOfTheRings.Classes.Entity (toAttrs)
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
import LordOfTheRings.Scenario.Types (scenarioName)
import Auth
import Model
import Network.HTTP.Types.Status (status400, status401, status409)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import UnliftIO.STM
import Yesod.Core
import Yesod.Persist.Core
import Yesod.WebSockets

data App = App
  { pool :: ConnectionPool
  , jwtSecret :: Text
  , channels :: TVar (Map LotrGameId (TChan LByteString))
  }

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /games GamesR GET POST
  /games/#LotrGameId GameR GET POST DELETE
  /games/#LotrGameId/undo UndoR POST
  /games/#LotrGameId/reset ResetR POST
  /cards CardsR GET
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
  maybe (sendResponseStatus status401 ("sign in first" :: Text)) pure $ toSqlKey <$> (readMaybe . unpack =<< subject)

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
  when (null username || null email) $ invalidArgs ["Choose a username and give an email address."]
  when (length reg.password < 8) $ invalidArgs ["Passwords must be at least 8 characters."]
  taken <- runDB $ (||) <$> (isJust <$> getBy (UniqueUsername username)) <*> (isJust <$> getBy (UniqueEmail email))
  when taken $ sendResponseStatus status409 ("That username or email is already registered." :: Text)
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
    _ -> sendResponseStatus status401 ("That username or password is not right." :: Text)

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

data NewGame = NewGame {name :: Text, playerName :: Text, seed :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

-- | Deal a new game from a seed and play it up to its first question.
startGame :: Text -> Int -> IO (Game, [Message], StdGen)
startGame playerName seed = do
  (player, heroes) <- either (error . show) pure $ evalRand (loadPlayer playerName starterDeck) (mkStdGen seed)
  chan <- newBroadcastTChanIO
  env <- envFor chan (newGame player heroes) [] (mkStdGen $ seed + 1)
  void $ runGameT env $ pushAll [Setup, BeginRound] >> mainLoop
  currentState env

postGamesR :: Handler Value
postGamesR = do
  userId <- requireUserId
  new <- requireCheckJsonBody @_ @NewGame
  (g, queue, gen) <- liftIO . startGame new.playerName =<< maybe (liftIO randomIO) pure new.seed
  now <- liftIO getCurrentTime
  gid <-
    runDB
      $ insert
        LotrGame
          { lotrGameUserId = userId
          , lotrGameName = new.name
          , lotrGamePlayerName = new.playerName
          , lotrGameCurrentData = toJSON g
          , lotrGameCurrentQueue = toJSON queue
          , lotrGameRandomGen = showGen gen
          , lotrGameStep = 0
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
  env <- loadGame gid row
  (before, queue, gen) <- liftIO $ currentState env
  when (answer.questionVersion /= before.questionVersion) $
    sendResponseStatus status409 ("stale answer" :: Text)
  void $ liftIO $ runGameT env $ answerQuestion answer.player answer.choice
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

-- | Step a game back one answer by applying the latest step's patch.
postUndoR :: LotrGameId -> Handler ()
postUndoR gid = do
  row <- requireOwnedGame gid
  mstep <- runDB $ getBy (UniqueGameStep gid row.lotrGameStep)
  for_ mstep \(Entity stepId step) -> do
    patch <- decodeOrFail "undo patch" step.lotrStepPatchDown
    previous <- case Diff.patch patch row.lotrGameCurrentData of
      Success v -> pure v
      Error err -> sendResponseStatus status400 ("could not undo: " <> pack err)
    g <- decodeOrFail "game" previous
    now <- liftIO getCurrentTime
    row' <- runDB do
      delete stepId
      updateGet
        gid
        [ LotrGameCurrentData =. previous
        , LotrGameCurrentQueue =. step.lotrStepQueue
        , LotrGameRandomGen =. step.lotrStepRandomGen
        , LotrGameStep =. row.lotrGameStep - 1
        , LotrGameUpdatedAt =. now
        ]
    broadcast gid row' g

postResetR :: LotrGameId -> Handler ()
postResetR gid = do
  row <- requireOwnedGame gid
  (g, queue, gen) <- liftIO . startGame row.lotrGamePlayerName =<< liftIO randomIO
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
  warp port App {pool, jwtSecret, channels}
