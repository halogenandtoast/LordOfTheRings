module LordOfTheRings.Location.Cards.HaradRoad (haradRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (addToOrcsArea, atOrcsStage, toOrcsArea)

newtype HaradRoad = HaradRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradRoad :: LocationCard HaradRoad
haradRoad = location HaradRoad Cards.haradRoad

-- | When Revealed: Add Harad Road to the Orcs' area.
-- Shadow: If the players are not at the same stage as the Orcs, move attacking enemy to
-- the Orcs' area after this attack.
instance RunMessage HaradRoad where
  runMessage msg l@(HaradRoad a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      addToOrcsArea a.id
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      unlessM atOrcsStage $ afterAttack eid [toOrcsArea eid]
      pure l
    _ -> HaradRoad <$> runMessage msg a
