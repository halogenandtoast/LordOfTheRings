module LordOfTheRings.Location.Cards.WindingCaverns (windingCaverns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under a Lost Island side.
newtype WindingCaverns = WindingCaverns LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

windingCaverns :: LocationCard WindingCaverns
windingCaverns = location WindingCaverns Cards.windingCaverns

-- | While Winding Caverns is the active location, each enemy gets +10 engagement cost.
-- When you would travel from Winding Caverns to another location, you may travel to a
-- location up to 3 locations away. (You must still pay that location's Travel cost.)
instance HasModifiersFor WindingCaverns where
  getModifiersFor (WindingCaverns a) = when (a.placement == ActiveLocation) do
    modifySelf a [TravelRange 3]
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [EngagementCostModifier 10]

instance RunMessage WindingCaverns where
  runMessage msg (WindingCaverns a) = WindingCaverns <$> runMessage msg a
