module LordOfTheRings.Treachery.Cards.DownFromTheHills (downFromTheHills) where

import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers (searchToRevealDunlandEnemy)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DownFromTheHills = DownFromTheHills TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

downFromTheHills :: TreacheryCard DownFromTheHills
downFromTheHills = treachery DownFromTheHills Cards.downFromTheHills

-- | If any player has 5 or more cards in hand, this effect cannot be canceled.
instance HasModifiersFor DownFromTheHills where
  getModifiersFor (DownFromTheHills a) =
    whenM (selectAny $ PlayerWithCardsInHand 5) $ modified a a [WhenRevealedCannotBeCanceled]

-- | When Revealed: Each player must choose: Either remove 1 time counter from the current
-- quest, or search the encounter deck and discard pile for a Dunland enemy, reveal it, and
-- add it to the staging area. Shuffle the encounter deck.
-- Each search is built as its player chooses it, after the players before him.
instance RunMessage DownFromTheHills where
  runMessage msg t@(DownFromTheHills attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseOneM p do
        questionLabeled "downFromTheHills.choose"
        causedBy attrs
        labeled "downFromTheHills.removeTimeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
        labeled "downFromTheHills.search" $ doStep 1 (WithTarget (toTarget p) msg)
      pure t
    DoStep 1 (WithTarget (PlayerTarget p) (WhenRevealed (isTarget attrs -> True))) -> do
      searchToRevealDunlandEnemy p attrs
      pure t
    _ -> DownFromTheHills <$> runMessage msg attrs
