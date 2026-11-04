module LordOfTheRings.Treachery.Cards.RoastEmOrBoilEm (roastEmOrBoilEm) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollInStaging)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RoastEmOrBoilEm = RoastEmOrBoilEm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

roastEmOrBoilEm :: TreacheryCard RoastEmOrBoilEm
roastEmOrBoilEm = treachery RoastEmOrBoilEm Cards.roastEmOrBoilEm

-- | When Revealed: Deal 1 damage to each ally. (2 damage instead if there is a Troll enemy
-- in the staging area.)
-- Shadow: Search the encounter deck for Troll Camp and add it to the staging area. Then,
-- shuffle the encounter deck.
instance RunMessage RoastEmOrBoilEm where
  runMessage msg t@(RoastEmOrBoilEm attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      troll <- trollInStaging
      allies <- select AnyAlly
      for_ allies \a -> dealDamage attrs a (if troll then 2 else 1)
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      lead <- getLead
      searchEncounterDeckToStaging lead (cardIs Locations.trollCamp)
      pure t
    _ -> RoastEmOrBoilEm <$> runMessage msg attrs
