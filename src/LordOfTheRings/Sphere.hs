module LordOfTheRings.Sphere (module LordOfTheRings.Sphere) where

import LordOfTheRings.Prelude

data Sphere = Leadership | Tactics | Spirit | Lore | Neutral
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
