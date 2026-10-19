module LordOfTheRings.Classes.HasGame (module LordOfTheRings.Classes.HasGame) where

import {-# SOURCE #-} LordOfTheRings.Game.Base
import LordOfTheRings.Prelude
import Control.Monad.Writer.Strict (WriterT)
import LordOfTheRings.Queue

class (Monad m) => HasGame m where
  getGame :: m Game

instance (HasGame m) => HasGame (ReaderT r m) where
  getGame = lift getGame

instance (HasGame m) => HasGame (QueueT msg m) where
  getGame = lift getGame

getsGame :: (HasGame m) => (Game -> a) -> m a
getsGame f = f <$> getGame

-- | Read a game without running it, for values derived from a stored game.
newtype ReadGame a = ReadGame {runReadGame :: Game -> a}
  deriving newtype (Functor, Applicative, Monad)

instance HasGame ReadGame where
  getGame = ReadGame id

instance (Monoid w, HasGame m) => HasGame (WriterT w m) where
  getGame = lift getGame
