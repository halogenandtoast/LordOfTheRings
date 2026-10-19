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
import Model
import Network.HTTP.Types.Status (status400, status409)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import UnliftIO.STM
import Yesod.Core
import Yesod.Persist.Core
import Yesod.WebSockets

data App = App
  { pool :: ConnectionPool
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
|]

instance Yesod App

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
  games <- runDB $ selectList [] [Desc LotrGameUpdatedAt]
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
  new <- requireCheckJsonBody @_ @NewGame
  (g, queue, gen) <- liftIO . startGame new.playerName =<< maybe (liftIO randomIO) pure new.seed
  now <- liftIO getCurrentTime
  gid <-
    runDB
      $ insert
        LotrGame
          { lotrGameName = new.name
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
  row <- runDB $ get404 gid
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
  answer <- requireCheckJsonBody @_ @Answer
  row <- runDB $ get404 gid
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
deleteGameR gid = runDB $ delete gid

-- | Step a game back one answer by applying the latest step's patch.
postUndoR :: LotrGameId -> Handler ()
postUndoR gid = do
  row <- runDB $ get404 gid
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
  row <- runDB $ get404 gid
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
  warp port App {pool, channels}
