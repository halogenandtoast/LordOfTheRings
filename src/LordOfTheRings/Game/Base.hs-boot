module LordOfTheRings.Game.Base where

import LordOfTheRings.Prelude

data Game

class (Monad m) => HasGame m where
  getGame :: m Game
