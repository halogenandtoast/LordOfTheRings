module LordOfTheRings.Classes.HasQueue (module LordOfTheRings.Classes.HasQueue) where

import LordOfTheRings.Prelude
import LordOfTheRings.Queue

class (MonadIO m) => HasQueue msg m | m -> msg where
  messageQueue :: m (Queue msg)
  pushAll :: [msg] -> m ()
  pushAll = withQueue_ . (<>)

instance (MonadIO m) => HasQueue msg (QueueT msg m) where
  messageQueue = ask
  pushAll (reverse -> msgs) = withQueue_ (msgs <>)

instance (HasQueue msg m) => HasQueue msg (ReaderT r m) where
  messageQueue = lift messageQueue
  pushAll = lift . pushAll

runQueueT :: (HasQueue msg m) => QueueT msg m a -> m a
runQueueT body = do
  inbox <- newIORef []
  a <- runReaderT (unQueueT body) (Queue inbox)
  msgs <- readIORef inbox
  pushAll $ reverse msgs
  pure a

evalQueueT :: (MonadIO m) => QueueT msg m a -> m [msg]
evalQueueT body = do
  inbox <- newIORef []
  _ <- runReaderT (unQueueT body) (Queue inbox)
  reverse <$> readIORef inbox

newQueue :: (MonadIO m) => [msg] -> m (Queue msg)
newQueue msgs = Queue <$> newIORef msgs

withQueue :: (HasQueue msg m) => ([msg] -> ([msg], r)) -> m r
withQueue body = do
  queue <- messageQueue
  atomicModifyIORef' (queueToRef queue) body

withQueue_ :: (HasQueue msg m) => ([msg] -> [msg]) -> m ()
withQueue_ body = withQueue ((,()) . body)

fromQueue :: (HasQueue msg m) => ([msg] -> r) -> m r
fromQueue f = f <$> (readIORef . queueToRef =<< messageQueue)

popMessage :: (HasQueue msg m) => m (Maybe msg)
popMessage = withQueue \case
  [] -> ([], Nothing)
  (m : ms) -> (ms, Just m)

clearQueue :: (HasQueue msg m) => m ()
clearQueue = withQueue_ $ const []

setQueue :: (HasQueue msg m) => [msg] -> m ()
setQueue = withQueue_ . const

peekMessage :: (HasQueue msg m) => m (Maybe msg)
peekMessage = fromQueue headMay

peekQueue :: (HasQueue msg m) => m [msg]
peekQueue = fromQueue id

push :: (HasQueue msg m) => msg -> m ()
push = pushAll . pure

pushEnd :: (HasQueue msg m) => msg -> m ()
pushEnd = pushAllEnd . pure

pushAllEnd :: (HasQueue msg m) => [msg] -> m ()
pushAllEnd msgs = withQueue_ (<> msgs)

pushWhen :: (HasQueue msg m) => Bool -> msg -> m ()
pushWhen b = when b . push

insertAfterMatching :: (HasQueue msg m) => [msg] -> (msg -> Bool) -> m ()
insertAfterMatching msgs p = withQueue_ \queue ->
  let (before, rest) = break p queue
   in case rest of
        (x : xs) -> before <> (x : msgs <> xs)
        _ -> before

popMessagesMatching :: (HasQueue msg m) => (msg -> Bool) -> m [msg]
popMessagesMatching p = withQueue \queue -> let (matched, rest) = partition p queue in (rest, matched)
