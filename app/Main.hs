{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Conduit
import Control.Monad.Random (
  MonadRandom (..),
  getRandom,
  newStdGen,
  mkStdGen,
  randomIO,
  randomRIO,
  randomRs,
  randoms,
 )
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally
import LordOfTheRings.Card
import LordOfTheRings.Game
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Decklist
import LordOfTheRings.GameT
import LordOfTheRings.Game qualified as E
import LordOfTheRings.Hero
import LordOfTheRings.Id
import LordOfTheRings.Message
import LordOfTheRings.Name
import LordOfTheRings.Prelude hiding (Handler)
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Scenario
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import UnliftIO.STM
import Network.HTTP.Types.Status (status409)
import Yesod.Core
import Yesod.WebSockets

data GameData = GameData
  { game :: Game
  , playerId :: PlayerId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data Room = Room
  { env :: GameEnv
  , chan :: TChan LByteString
  }

data App = App {room :: Room}

mkYesod
  "App"
  [parseRoutes|
/api ApiP:
  /data GameR GET POST
  /reset ResetR POST
|]

instance Yesod App

instance ToTypedContent GameData where
  toTypedContent = toTypedContent . toJSON

instance ToContent GameData where
  toContent = toContent . toJSON

instance MonadRandom (HandlerFor App) where
  getRandom = liftIO randomIO
  getRandomR = liftIO . randomRIO
  getRandomRs lohi = fmap (randomRs lohi) newStdGen
  getRandoms = fmap randoms newStdGen

handleSocket :: Room -> WebSocketsT Handler ()
handleSocket room = do
  readChannel <- atomically $ dupTChan room.chan
  race_
    (forever $ atomically (readTChan readChannel) >>= sendTextData)
    (runConduit $ sourceWS .| mapM_C (atomically . writeTChan room.chan))

getGameR :: Handler GameData
getGameR = do
  App {room} <- getYesod
  webSockets (handleSocket room)
  g <- liftIO $ runGameT room.env mainLoop
  pure $ GameData g g.activePlayer

data Answer = Answer
  { choice :: Int
  , player :: PlayerId
  , questionVersion :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

postGameR :: Handler ()
postGameR = do
  App {room} <- getYesod
  answer <- requireCheckJsonBody @_ @Answer
  current <- liftIO $ runGameT room.env getGame
  when (answer.questionVersion /= current.questionVersion) $
    sendResponseStatus status409 ("stale answer" :: Text)
  g <- liftIO $ runGameT room.env $ answerQuestion answer.player answer.choice
  liftIO $
    atomically $
      writeTChan room.chan $
        encode $
          toJSON $
            object ["contents" .= GameData g g.activePlayer, "tag" .= String "Update"]

postResetR :: Handler ()
postResetR = do
  App {room} <- getYesod
  liftIO $ runGameT room.env $ do
    clearQueue
    resetGame
    pushAll [Setup, BeginRound, EndRound]
    g <- mainLoop
    atomically $
      writeTChan room.chan $
        encode $
          toJSON $
            object ["contents" .= GameData g g.activePlayer, "tag" .= String "Update"]

resetGame :: GameT ()
resetGame = either (error . show) (putGame . uncurry newGame) =<< loadPlayer starterDeck

main :: IO ()
main = do
  (player, heroes) <- either (error . show) pure =<< loadPlayer starterDeck
  c <- atomically newTChan
  let handler msg = atomically $ writeTChan c (encode $ toJSON msg)
  env <- (\e -> e {gameDebug = 1}) <$> (newGameEnv (newGame player heroes) handler =<< randomIO)
  let room = Room {env, chan = c}
  runGameT env (pushAll [Setup, BeginRound, EndRound])
  port <- fromMaybe 3000 . (>>= readMaybe) <$> lookupEnv "PORT"
  warp port (App room)
