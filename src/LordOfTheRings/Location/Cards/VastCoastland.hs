module LordOfTheRings.Location.Cards.VastCoastland (vastCoastland) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (addToStormcallerArea, atStormcallersStage)

newtype VastCoastland = VastCoastland LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

vastCoastland :: LocationCard VastCoastland
vastCoastland = location VastCoastland Cards.vastCoastland

-- | When Revealed: Add Vast Coastland to the Stormcaller's area.
-- Shadow: Attacking enemy gets +2 attack if you are at the same stage as the Stormcaller.
instance RunMessage VastCoastland where
  runMessage msg l@(VastCoastland a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      addToStormcallerArea (toTarget a)
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      whenM atStormcallersStage $ enemyGetsAttack a eid 2
      pure l
    _ -> VastCoastland <$> runMessage msg a
