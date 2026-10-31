module LordOfTheRings.Treachery.Cards.Quicksand (quicksand) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Quicksand = Quicksand TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

quicksand :: TreacheryCard Quicksand
quicksand = treachery Quicksand Cards.quicksand

-- | When Revealed: Either remove 1 time counter from the current quest or remove each
-- character with 3 or more willpower from the quest.
-- Shadow: Defending character cannot ready until the end of the round.
instance RunMessage Quicksand where
  runMessage msg t@(Quicksand attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      quest <- getCurrentQuest
      counters <- maybe (pure 0) getTimeCounters quest
      strong <- filterM (fmap (>= 3) . getWillpower) =<< selectCharacters CommittedHero CommittedAlly
      lead <- getLead
      chooseOneM lead do
        questionLabeled "quicksand.choose"
        causedBy attrs
        when (counters > 0) $ labeled "quicksand.removeTimeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
        labeled "quicksand.removeFromQuest" $ traverse_ removeFromQuest strong
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> roundModifier attrs d CannotReady
      pure t
    _ -> Quicksand <$> runMessage msg attrs
