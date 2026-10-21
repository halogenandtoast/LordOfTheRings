module LordOfTheRings.Location.Cards.OakWoodGrove (oakWoodGrove) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Sphere (Sphere (Lore))

newtype OakWoodGrove = OakWoodGrove LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

oakWoodGrove :: LocationCard OakWoodGrove
oakWoodGrove = location OakWoodGrove Cards.oakWoodGrove

-- | While Oak-wood Grove is the active location, resource tokens from any sphere may be
-- spent as Lore resource tokens.
instance HasModifiersFor OakWoodGrove where
  getModifiersFor (OakWoodGrove a) = when (a.placement == ActiveLocation) $ modified a GameTarget [ResourcesSpendableAs Lore]
