{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad.Random (StdGen, evalRand, mkStdGen, randomIO)
import Data.Map.Strict qualified as Map
import Data.UUID qualified as UUID
import LordOfTheRings.Card (allEncounterCards, allPlayerCards)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Decklist
import LordOfTheRings.Game
import LordOfTheRings.Game.Public
import LordOfTheRings.GameT
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Prelude hiding (Handler)
import LordOfTheRings.Scenario.Types (scenarioName)
import Network.HTTP.Types.Status (status404, status409)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import UnliftIO.STM
import Yesod.Core
import Yesod.WebSockets

-- | What undo restores: the game, the messages still queued and the random generator.
data Snapshot = Snapshot Game [Message] StdGen

data Session = Session
  { env :: GameEnv
  , name :: Text
  , playerName :: Text
  , history :: [Snapshot]
  , chan :: TChan LByteString
  }

newtype App = App {sessions :: MVar (Map Text Session)}

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /games GamesR GET POST
  /games/#Text GameR GET POST DELETE
  /games/#Text/undo UndoR POST
  /games/#Text/reset ResetR POST
  /cards CardsR GET
|]

instance Yesod App

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
  { id :: Text
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

gameData :: Session -> IO GameData
gameData s = do
  g <- runGameT s.env getGame
  pure $ GameData True (Just $ publicGame g) (Just g.activePlayer) (notNull s.history)

summary :: Text -> Session -> IO GameSummary
summary gid s = do
  g <- runGameT s.env getGame
  pure
    GameSummary
      { id = gid
      , name = s.name
      , playerName = s.playerName
      , scenario = scenarioName (toAttrs g.scenario)
      , round = g.round
      , phase = g.phase
      , threat = sum [p.threat | p <- toList g.players]
      , outcome = g.outcome
      }

broadcast :: Session -> IO ()
broadcast s = do
  current <- gameData s
  atomically $ writeTChan s.chan $ encode $ object ["tag" .= String "Update", "contents" .= current]

findSession :: Text -> Handler Session
findSession gid = do
  app <- getYesod
  maybe (sendResponseStatus status404 ("no such game" :: Text)) pure . Map.lookup gid =<< liftIO (readMVar app.sessions)

-- | Run a change against one game and tell its clients about the result.
withSession :: Text -> (Session -> IO Session) -> Handler ()
withSession gid f = do
  app <- getYesod
  found <- liftIO $ modifyMVar app.sessions \sessions -> case Map.lookup gid sessions of
    Nothing -> pure (sessions, False)
    Just s -> do
      s' <- f s
      broadcast s'
      pure (Map.insert gid s' sessions, True)
  unless found $ sendResponseStatus status404 ("no such game" :: Text)

newSession :: Text -> Text -> Int -> IO Session
newSession name playerName seed = do
  (player, heroes) <- either (error . show) pure $ evalRand (loadPlayer playerName starterDeck) (mkStdGen seed)
  chan <- newBroadcastTChanIO
  let logger msg = atomically $ writeTChan chan $ encode $ toJSON msg
  env <- (\e -> e {gameDebug = 1}) <$> newGameEnv (newGame player heroes) logger (seed + 1)
  void $ runGameT env $ pushAll [Setup, BeginRound] >> mainLoop
  pure Session {env, name, playerName, history = [], chan}

snapshot :: GameEnv -> IO Snapshot
snapshot env = Snapshot <$> runGameT env getGame <*> runGameT env peekQueue <*> readIORef (gameRandomGen env)

restore :: GameEnv -> Snapshot -> IO ()
restore env (Snapshot g queue gen) = do
  runGameT env $ putGame g >> setQueue queue
  writeIORef (gameRandomGen env) gen

getGamesR :: Handler Value
getGamesR = do
  app <- getYesod
  sessions <- liftIO $ readMVar app.sessions
  toJSON <$> liftIO (traverse (uncurry summary) (Map.toList sessions))

data NewGame = NewGame {name :: Text, playerName :: Text, seed :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGamesR :: Handler Value
postGamesR = do
  app <- getYesod
  new <- requireCheckJsonBody @_ @NewGame
  gid <- UUID.toText <$> liftIO randomIO
  s <- liftIO $ newSession new.name new.playerName =<< maybe randomIO pure new.seed
  liftIO $ modifyMVar_ app.sessions (pure . Map.insert gid s)
  pure $ object ["id" .= gid]

handleSocket :: Session -> WebSocketsT Handler ()
handleSocket s = do
  readChannel <- atomically $ dupTChan s.chan
  forever $ atomically (readTChan readChannel) >>= sendTextData

getGameR :: Text -> Handler Value
getGameR gid = do
  s <- findSession gid
  webSockets (handleSocket s)
  toJSON <$> liftIO (gameData s)

data Answer = Answer
  { choice :: Int
  , player :: PlayerId
  , questionVersion :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGameR :: Text -> Handler ()
postGameR gid = do
  answer <- requireCheckJsonBody @_ @Answer
  s <- findSession gid
  g <- liftIO $ runGameT s.env getGame
  when (answer.questionVersion /= g.questionVersion) $
    sendResponseStatus status409 ("stale answer" :: Text)
  withSession gid \s' -> do
    before <- snapshot s'.env
    void $ runGameT s'.env $ answerQuestion answer.player answer.choice
    pure s' {history = before : s'.history}

deleteGameR :: Text -> Handler ()
deleteGameR gid = do
  app <- getYesod
  liftIO $ modifyMVar_ app.sessions (pure . Map.delete gid)

postUndoR :: Text -> Handler ()
postUndoR gid = withSession gid \s -> case s.history of
  (previous : rest) -> do
    restore s.env previous
    pure s {history = rest}
  [] -> pure s

postResetR :: Text -> Handler ()
postResetR gid = withSession gid \s -> do
  fresh <- newSession s.name s.playerName =<< randomIO
  pure fresh {chan = s.chan}

getCardsR :: Handler Value
getCardsR = pure $ toJSON $ allPlayerCards <> allEncounterCards

main :: IO ()
main = do
  sessions <- newMVar mempty
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
  warp port App {sessions}
