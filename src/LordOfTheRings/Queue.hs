module LordOfTheRings.Queue (module LordOfTheRings.Queue) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Sequence (Seq, (|>), data Empty, data (:<|))
import LordOfTheRings.Message (Message)
import LordOfTheRings.Prelude

type Queue = (IORef (Seq Message), IORef (Seq Message))

clearQueue :: (HasQueue m) => m ()
clearQueue = do
  (readRef, writeRef) <- getQueue
  liftIO $ do
    atomicModifyIORef' readRef $ const (Empty, ())
    atomicModifyIORef' writeRef $ const (Empty, ())

newQueue :: (MonadIO m) => m Queue
newQueue = liftIO $ do
  inbox <- newIORef Empty
  queue <- newIORef Empty
  pure (inbox, queue)

push :: (HasQueue m) => Message -> m ()
push msg = do
  (ref, _) <- getQueue
  liftIO $ atomicModifyIORef' ref $ \inbox -> (inbox |> msg, ())

flushQueue :: (HasQueue m) => m (Seq Message)
flushQueue = do
  (readRef, writeRef) <- getQueue
  liftIO $ do
    newMsgs <- atomicModifyIORef' readRef (Empty,)
    atomicModifyIORef' writeRef $ \oldMsgs -> let msgs' = newMsgs <> oldMsgs in (msgs', msgs')

peekQueue :: (HasQueue m) => m (Seq Message)
peekQueue = flushQueue

pushAll :: (HasQueue m) => [Message] -> m ()
pushAll = traverse_ push

pop :: (HasQueue m) => m (Maybe Message)
pop = do
  (readRef, writeRef) <- getQueue
  liftIO $ do
    newMsgs <- atomicModifyIORef' readRef (Empty,)
    atomicModifyIORef' writeRef $ \oldMsgs -> case newMsgs <> oldMsgs of
      Empty -> (Empty, Nothing)
      msg :<| msgs -> (msgs, Just msg)

class (MonadIO m) => HasQueue m where
  getQueue :: m Queue
