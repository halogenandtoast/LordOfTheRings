module LordOfTheRings.Run (module LordOfTheRings.Run) where

import LordOfTheRings.Frontend
import {-# SOURCE #-} LordOfTheRings.Game.Base
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.HasQueue

class Run a where
  run :: (HasQueue Message m, HasGame m, MonadRandom m, Frontend m) => Message -> a -> m a
