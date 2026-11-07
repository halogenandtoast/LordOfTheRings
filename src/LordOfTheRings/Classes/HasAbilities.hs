module LordOfTheRings.Classes.HasAbilities (module LordOfTheRings.Classes.HasAbilities) where

import LordOfTheRings.Ability.Types

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []

  -- | Text explicitly active while the physical card is in the victory display.
  getVictoryAbilities :: a -> [Ability]
  getVictoryAbilities _ = []
