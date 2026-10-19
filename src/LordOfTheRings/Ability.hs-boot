module LordOfTheRings.Ability (module LordOfTheRings.Ability) where

import LordOfTheRings.Prelude
import LordOfTheRings.Source

data Ability

instance Show Ability
instance FromJSON Ability
instance ToJSON Ability

abilitySource :: Ability -> Source
abilityIndex :: Ability -> Int

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []
