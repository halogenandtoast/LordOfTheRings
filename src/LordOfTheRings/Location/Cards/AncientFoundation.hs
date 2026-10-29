module LordOfTheRings.Location.Cards.AncientFoundation (ancientFoundation) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (assignDamageAmongLocations, isScour)

newtype AncientFoundation = AncientFoundation LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientFoundation :: LocationCard AncientFoundation
ancientFoundation = location AncientFoundation Cards.ancientFoundation

-- | Scour: Assign X damage among locations in play. X is the number of locations in play.
-- This ability does not stack with other copies of Ancient Foundation: the trigger takes
-- only one copy's.
-- Travel: Place 1 damage on this location to travel here.
instance RunMessage AncientFoundation where
  runMessage msg l@(AncientFoundation a) = case msg of
    _ | isScour a msg -> do
      x <- length <$> select AnyLocation
      assignDamageAmongLocations a x
      pure l
    TravelTo lid | lid == a.id -> do
      placeDamage a a 1
      pure l
    _ -> AncientFoundation <$> runMessage msg a
