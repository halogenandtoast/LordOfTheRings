module LordOfTheRings.Location.Cards.PelargirDocks (pelargirDocks) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype PelargirDocks = PelargirDocks LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelargirDocks :: LocationCard PelargirDocks
pelargirDocks = location PelargirDocks Cards.pelargirDocks

-- | While Pelargir Docks is the active location, enemies get +1 attack and +1 defense.
instance HasModifiersFor PelargirDocks where
  getModifiersFor (PelargirDocks a) = when (a.placement == ActiveLocation) do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [AttackModifier 1, DefenseModifier 1]
