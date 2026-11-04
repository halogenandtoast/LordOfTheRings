module LordOfTheRings.Treachery.Cards.StrickenDumb (strickenDumb) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StrickenDumb = StrickenDumb TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril.
strickenDumb :: TreacheryCard StrickenDumb
strickenDumb = treachery StrickenDumb Cards.strickenDumb

-- | When Revealed: You must either shuffle the top card of the burden deck into the
-- encounter deck and reveal an additional encounter card, or raise each player's threat by
-- 3 and immediately end the quest phase (do not resolve the quest). With the Burden deck
-- empty only the second can be done in full, so it is the one left.
instance RunMessage StrickenDumb where
  runMessage msg t@(StrickenDumb attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      you <- getRevealer
      burdens <- getBurdenDeck
      chooseOrRunOneM you do
        questionLabeled "strickenDumb.choose"
        causedBy attrs
        when (notNull burdens) $ labeled "strickenDumb.burden" do
          shuffleTopOfBurdenDeckIntoEncounterDeck
          revealEncounterCard
        labeled "strickenDumb.threat" do
          eachPlayer \pid -> encounterRaisesThreat pid 3
          doStep 1 msg
      pure t
    -- the quest phase ends as the choice resolves, not as it is offered
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      endQuestPhaseWithoutResolving
      pure t
    _ -> StrickenDumb <$> runMessage msg attrs
