module LordOfTheRings.Classes.HasAbilities (module LordOfTheRings.Classes.HasAbilities) where

import LordOfTheRings.Ability.Types

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []
