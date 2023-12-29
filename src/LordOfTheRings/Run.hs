module LordOfTheRings.Run (module LordOfTheRings.Run) where

import Control.Monad.Random (MonadRandom)
import LordOfTheRings.Frontend
import {-# SOURCE #-} LordOfTheRings.Game.Base
import LordOfTheRings.Message
import LordOfTheRings.Queue

class Run a where
  run :: (HasQueue m, HasGame m, MonadRandom m, Frontend m) => Message -> a -> m a
