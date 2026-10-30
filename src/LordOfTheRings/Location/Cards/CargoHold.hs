module LordOfTheRings.Location.Cards.CargoHold (cargoHold) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype CargoHold = CargoHold LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cargoHold :: LocationCard CargoHold
cargoHold = location CargoHold Cards.cargoHold

-- | While Cargo Hold is the active location, each enemy gets +1 defense.
instance HasModifiersFor CargoHold where
  getModifiersFor (CargoHold a) = when (a.placement == ActiveLocation) do
    enemies <- select AnyEnemy
    for_ enemies \e -> modified a e [DefenseModifier 1]

-- | Shadow: Until the end of the phase, attacking enemy gets +2 defense.
instance RunMessage CargoHold where
  runMessage msg l@(CargoHold attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (DefenseModifier 2)
      pure l
    _ -> CargoHold <$> runMessage msg attrs
