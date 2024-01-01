module LordOfTheRings.Ability (module LordOfTheRings.Ability) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Ability

instance Show Ability
instance FromJSON Ability
instance ToJSON Ability

toAbilityRef :: Ability -> AbilityRef

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []
