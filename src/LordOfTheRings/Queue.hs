module LordOfTheRings.Queue (module LordOfTheRings.Queue) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Sequence (Seq, (|>), pattern Empty, pattern (:<|))
import LordOfTheRings.Message (Message)
import LordOfTheRings.Prelude

type Queue = (IORef (Seq Message), IORef (Seq Message))

clearQueue :: (HasQueue m) => m ()
clearQueue = do
  (readRef, writeRef) <- getQueue
  liftIO $ do
    atomicModifyIORef' readRef $ \_ -> (Empty, ())
    atomicModifyIORef' writeRef $ \_ -> (Empty, ())

newQueue :: (MonadIO m) => m Queue
newQueue = liftIO $ do
  inbox <- newIORef Empty
  queue <- newIORef Empty
  pure (inbox, queue)

push :: (HasQueue m) => Message -> m ()
push msg = do
  (ref, _) <- getQueue
  liftIO $ atomicModifyIORef' ref $ \inbox -> (inbox |> msg, ())

pushAll :: (HasQueue m) => [Message] -> m ()
pushAll msgs = traverse_ push msgs

pop :: (HasQueue m) => m (Maybe Message)
pop = do
  (readRef, writeRef) <- getQueue
  liftIO $ do
    newMsgs <- atomicModifyIORef' readRef $ \inbox -> (Empty, inbox)
    atomicModifyIORef' writeRef $ \oldMsgs -> case newMsgs <> oldMsgs of
      Empty -> (Empty, Nothing)
      msg :<| msgs -> (msgs, Just msg)

class (MonadIO m) => HasQueue m where
  getQueue :: m Queue
