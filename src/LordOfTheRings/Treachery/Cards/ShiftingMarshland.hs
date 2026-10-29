module LordOfTheRings.Treachery.Cards.ShiftingMarshland (shiftingMarshland) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ShiftingMarshland = ShiftingMarshland TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shiftingMarshland :: TreacheryCard ShiftingMarshland
shiftingMarshland = treachery ShiftingMarshland Cards.shiftingMarshland

-- | When Revealed: Remove 1 time counter from the current quest.
-- Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage ShiftingMarshland where
  runMessage msg t@(ShiftingMarshland attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      afterAttack eid =<< capture (immediateAttack pid eid)
      pure t
    _ -> ShiftingMarshland <$> runMessage msg attrs
