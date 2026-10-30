module LordOfTheRings.Treachery.Cards.AncientDepths (ancientDepths) where

import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype AncientDepths = AncientDepths TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientDepths :: TreacheryCard AncientDepths
ancientDepths = treachery AncientDepths Cards.ancientDepths

-- | This effect cannot be canceled.
instance HasModifiersFor AncientDepths where
  getModifiersFor (AncientDepths a) = modified a a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Add the top location from the Grotto deck to the staging area and flip
-- that location to its Underwater side.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the active location is
-- Underwater).
instance RunMessage AncientDepths where
  runMessage msg t@(AncientDepths attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      addUnderwaterFromGrottoDeck
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      underwater <- underwaterActive
      enemyGetsAttack (CardIdSource cid) eid (if underwater then 2 else 1)
      pure t
    _ -> AncientDepths <$> runMessage msg attrs
