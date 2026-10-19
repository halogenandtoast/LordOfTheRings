module LordOfTheRings.Queue (module LordOfTheRings.Queue) where

import LordOfTheRings.Prelude

newtype Queue msg = Queue {queueToRef :: IORef [msg]}

newtype QueueT msg m a = QueueT {unQueueT :: ReaderT (Queue msg) m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadRandom
    , MonadReader (Queue msg)
    , MonadTrans
    )
