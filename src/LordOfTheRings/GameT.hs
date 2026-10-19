{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.GameT (module LordOfTheRings.GameT) where

import Control.Monad.Random (MonadRandom (..), StdGen, mkStdGen, random, randomR, randomRs, randoms, splitGen)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Frontend
import {-# SOURCE #-} LordOfTheRings.Game.Base
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Queue

data GameEnv = GameEnv
  { gameEnvGame :: IORef Game
  , gameEnvQueue :: Queue Message
  , gameRandomGen :: IORef StdGen
  , gameLogger :: FrontendMessage -> IO ()
  , gameDebug :: Int
  }

newtype GameT a = GameT {unGameT :: ReaderT GameEnv IO a}
  deriving newtype (MonadReader GameEnv, Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

newGameEnv :: Game -> (FrontendMessage -> IO ()) -> Int -> IO GameEnv
newGameEnv game logger seed = do
  gameRef <- newIORef game
  queue <- newQueue []
  gen <- newIORef (mkStdGen seed)
  pure
    GameEnv
      { gameEnvGame = gameRef
      , gameEnvQueue = queue
      , gameRandomGen = gen
      , gameLogger = logger
      , gameDebug = 0
      }

runGameT :: GameEnv -> GameT a -> IO a
runGameT env body = runReaderT (unGameT body) env

instance HasGame GameT where
  getGame = asks gameEnvGame >>= readIORef

instance HasQueue Message GameT where
  messageQueue = asks gameEnvQueue

instance Frontend GameT where
  send msg = asks gameLogger >>= \f -> liftIO (f msg)

instance MonadRandom GameT where
  getRandomR lohi = do
    ref <- asks gameRandomGen
    atomicModifyIORef' ref (swap . randomR lohi)
  getRandom = do
    ref <- asks gameRandomGen
    atomicModifyIORef' ref (swap . random)
  getRandomRs lohi = do
    ref <- asks gameRandomGen
    gen <- atomicModifyIORef' ref splitGen
    pure $ randomRs lohi gen
  getRandoms = do
    ref <- asks gameRandomGen
    gen <- atomicModifyIORef' ref splitGen
    pure $ randoms gen

putGame :: Game -> GameT ()
putGame game = do
  ref <- asks gameEnvGame
  writeIORef ref game

debug :: Int -> IO () -> GameT ()
debug lvl action = do
  lvl' <- asks gameDebug
  when (lvl' >= lvl) $ liftIO action
