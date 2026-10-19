{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad.Random (StdGen, evalRand, mkStdGen, randomIO)
import LordOfTheRings.Card (allEncounterCards, allPlayerCards)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Decklist
import LordOfTheRings.Game
import LordOfTheRings.GameT
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Prelude hiding (Handler)
import Network.HTTP.Types.Status (status400, status409)
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
  , history :: [Snapshot]
  }

data App = App
  { session :: MVar (Maybe Session)
  , chan :: TChan LByteString
  }

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /data GameR GET POST
  /start StartR POST
  /undo UndoR POST
  /reset ResetR POST
  /cards CardsR GET
|]

instance Yesod App

data GameData = GameData
  { started :: Bool
  , game :: Maybe Game
  , playerId :: Maybe PlayerId
  , canUndo :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

instance ToContent GameData where
  toContent = toContent . toJSON

instance ToTypedContent GameData where
  toTypedContent = toTypedContent . toJSON

gameData :: Maybe Session -> IO GameData
gameData = \case
  Nothing -> pure $ GameData False Nothing Nothing False
  Just s -> do
    g <- runGameT s.env getGame
    pure $ GameData True (Just g) (Just g.activePlayer) (notNull s.history)

broadcast :: App -> Maybe Session -> IO ()
broadcast app s = do
  current <- gameData s
  atomically $ writeTChan app.chan $ encode $ object ["tag" .= String "Update", "contents" .= current]

handleSocket :: App -> WebSocketsT Handler ()
handleSocket app = do
  readChannel <- atomically $ dupTChan app.chan
  forever $ atomically (readTChan readChannel) >>= sendTextData

getGameR :: Handler GameData
getGameR = do
  app <- getYesod
  webSockets (handleSocket app)
  liftIO $ gameData =<< readMVar app.session

-- | Run a change against the session and tell every client about the result.
withSession :: (Maybe Session -> IO (Maybe Session)) -> Handler ()
withSession f = do
  app <- getYesod
  liftIO $ modifyMVar_ app.session \s -> do
    s' <- f s
    broadcast app s'
    pure s'

newSession :: App -> Text -> Int -> IO Session
newSession app name seed = do
  (player, heroes) <- either (error . show) pure $ evalRand (loadPlayer name starterDeck) (mkStdGen seed)
  let logger msg = atomically $ writeTChan app.chan $ encode $ toJSON msg
  env <- (\e -> e {gameDebug = 1}) <$> newGameEnv (newGame player heroes) logger (seed + 1)
  void $ runGameT env $ pushAll [Setup, BeginRound] >> mainLoop
  pure $ Session env name []

snapshot :: GameEnv -> IO Snapshot
snapshot env = Snapshot <$> runGameT env getGame <*> runGameT env peekQueue <*> readIORef (gameRandomGen env)

restore :: GameEnv -> Snapshot -> IO ()
restore env (Snapshot g queue gen) = do
  runGameT env $ putGame g >> setQueue queue
  writeIORef (gameRandomGen env) gen

data Start = Start {name :: Text, seed :: Maybe Int}
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postStartR :: Handler ()
postStartR = do
  app <- getYesod
  start <- requireCheckJsonBody @_ @Start
  withSession \_ -> Just <$> (newSession app start.name =<< maybe randomIO pure start.seed)

data Answer = Answer
  { choice :: Int
  , player :: PlayerId
  , questionVersion :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGameR :: Handler ()
postGameR = do
  app <- getYesod
  answer <- requireCheckJsonBody @_ @Answer
  current <- liftIO $ readMVar app.session
  s <- maybe (sendResponseStatus status400 ("no game" :: Text)) pure current
  g <- liftIO $ runGameT s.env getGame
  when (answer.questionVersion /= g.questionVersion) $
    sendResponseStatus status409 ("stale answer" :: Text)
  withSession \case
    Nothing -> pure Nothing
    Just s' -> do
      before <- snapshot s'.env
      void $ runGameT s'.env $ answerQuestion answer.player answer.choice
      pure $ Just s' {history = before : s'.history}

postUndoR :: Handler ()
postUndoR = withSession \case
  Just s | (previous : rest) <- s.history -> do
    restore s.env previous
    pure $ Just s {history = rest}
  other -> pure other

postResetR :: Handler ()
postResetR = do
  app <- getYesod
  withSession \case
    Nothing -> pure Nothing
    Just s -> Just <$> (newSession app s.name =<< randomIO)

getCardsR :: Handler Value
getCardsR = pure $ toJSON $ allPlayerCards <> allEncounterCards

main :: IO ()
main = do
  session <- newMVar Nothing
  chan <- atomically newTChan
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
  warp port App {session, chan}
