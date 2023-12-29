module LordOfTheRings.Game.Base where

import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Game
data Player

class (Monad m) => HasGame m where
  getGame :: m Game

getsGame :: (HasGame m) => (Game -> a) -> m a
getPlayer :: (HasGame m) => PlayerId -> m Player
