module LordOfTheRings.Location.Cards.TheTrollsCamp (theTrollsCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TheTrollsCamp = TheTrollsCamp LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTrollsCamp :: LocationCard TheTrollsCamp
theTrollsCamp = location TheTrollsCamp Cards.theTrollsCamp

-- | While The Troll's Camp is the active location, each hero gets +1 willpower, +1 attack
-- and +1 defense.
instance HasModifiersFor TheTrollsCamp where
  getModifiersFor (TheTrollsCamp a) = when (a.placement == ActiveLocation) do
    heroes <- select AnyHero
    for_ heroes \h -> modified a h [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]

instance RunMessage TheTrollsCamp where
  runMessage msg (TheTrollsCamp a) = TheTrollsCamp <$> runMessage msg a
