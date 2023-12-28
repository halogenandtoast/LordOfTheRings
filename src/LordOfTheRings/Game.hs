module LordOfTheRings.Game where

import Data.Aeson
import Data.Map.Strict (Map)
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Hero.Type
import LordOfTheRings.Id

class HasGame m where
  getGame :: m Game

newtype Game = Game {players :: Map PlayerId Player}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Player = Player
  { id :: PlayerId
  , heroes :: [Hero]
  , hand :: [SomeCard]
  , deck :: [SomeCard]
  , threat :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
