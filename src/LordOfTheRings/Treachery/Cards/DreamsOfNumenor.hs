module LordOfTheRings.Treachery.Cards.DreamsOfNumenor (dreamsOfNumenor) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DreamsOfNumenor = DreamsOfNumenor TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dreamsOfNumenor :: TreacheryCard DreamsOfNumenor
dreamsOfNumenor = treachery DreamsOfNumenor Cards.dreamsOfNumenor

-- | When Revealed: Either set your heading to the worst setting or exhaust all allies in
-- play. The first player chooses.
-- Shadow: If you are off-course, attacking enemy makes an additional attack after this one.
instance RunMessage DreamsOfNumenor where
  runMessage msg t@(DreamsOfNumenor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      allies <- select ReadyAlly
      chooseOneM lead do
        questionLabeled "dreamsOfNumenor.choose"
        causedBy attrs
        labeled "dreamsOfNumenor.worst" $ setHeadingToWorst attrs
        labeled "dreamsOfNumenor.exhaust" $ traverse_ exhaust allies
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM isOffCourse $ additionalAttack pid eid
      pure t
    _ -> DreamsOfNumenor <$> runMessage msg attrs
