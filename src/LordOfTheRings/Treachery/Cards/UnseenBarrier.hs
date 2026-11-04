module LordOfTheRings.Treachery.Cards.UnseenBarrier (unseenBarrier) where

import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (increasePursuit)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnseenBarrier = UnseenBarrier TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unseenBarrier :: TreacheryCard UnseenBarrier
unseenBarrier = treachery UnseenBarrier Cards.unseenBarrier

{- | When Revealed: The players cannot place more than 3 progress on the current quest this
round. (This does not count progress placed on any active location.)
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, increase the
pursuit value by 1.
-}
instance RunMessage UnseenBarrier where
  runMessage msg t@(UnseenBarrier attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      current <- getCurrentQuest
      for_ current \q -> roundModifier attrs q (ProgressPerRoundLimit 3)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      increasePursuit 1
      pure t
    _ -> UnseenBarrier <$> runMessage msg attrs
