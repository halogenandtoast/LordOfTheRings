module LordOfTheRings.Location.Cards.WasteOfDagorlad (wasteOfDagorlad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype WasteOfDagorlad = WasteOfDagorlad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wasteOfDagorlad :: LocationCard WasteOfDagorlad
wasteOfDagorlad = location WasteOfDagorlad Cards.wasteOfDagorlad

-- | While Waste of Dagorlad is the active location, treat the printed text box of each ally
-- as if it were blank (except for Traits).
instance HasModifiersFor WasteOfDagorlad where
  getModifiersFor (WasteOfDagorlad a) = when (a.placement == ActiveLocation) do
    allies <- select AnyAlly
    for_ allies \ally -> modified a ally [Blank]

instance RunMessage WasteOfDagorlad where
  runMessage msg (WasteOfDagorlad a) = WasteOfDagorlad <$> runMessage msg a
