module LordOfTheRings.Treachery.Cards.DangerCloseBehind (dangerCloseBehind) where

import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (randomFromLargestSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DangerCloseBehind = DangerCloseBehind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1, as its card def says.
dangerCloseBehind :: TreacheryCard DangerCloseBehind
dangerCloseBehind = treachery DangerCloseBehind Cards.dangerCloseBehind

{- | When Revealed: Choose 1 random card from the set aside encounter set with the most
cards remaining. Reveal that card as if it was the top card of the encounter deck.
Shadow: If this attack destroys a character, resolve the 'when revealed' effect on this
card.
-}
instance RunMessage DangerCloseBehind where
  runMessage msg t@(DangerCloseBehind attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealRandomFromLargest
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      revealRandomFromLargest
      pure t
    _ -> DangerCloseBehind <$> runMessage msg attrs
   where
    revealRandomFromLargest = traverse_ revealSetAsideCard =<< randomFromLargestSetAside
