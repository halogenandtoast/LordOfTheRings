module LordOfTheRings.Treachery.Cards.TakeCover (takeCover) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TakeCover = TakeCover TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

takeCover :: TreacheryCard TakeCover
takeCover = treachery TakeCover Cards.takeCover

-- | Surge.
-- When Revealed: Either deal 1 damage to each exhausted character, or remove 1 time counter
-- from the current quest.
instance RunMessage TakeCover where
  runMessage msg t@(TakeCover attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      timed <- canRemoveTimeCounterFromCurrentQuest
      lead <- getLead
      -- either one may be chosen, but one that can be done in full must be (FAQ 1.44)
      chooseOrRunOneM lead do
        questionLabeled "takeCover.choose"
        causedBy attrs
        unless (null exhausted) $ labeled "takeCover.damage" $ for_ exhausted \c -> dealDamage attrs c 1
        when timed $ labeled "removeTimeCounterFromQuest" $ removeTimeCountersFromCurrentQuest attrs 1
      pure t
    _ -> TakeCover <$> runMessage msg attrs
