module LordOfTheRings.Treachery.Cards.FalseLead (falseLead) where

import LordOfTheRings.Trait (Trait (Clue))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FalseLead = FalseLead TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

falseLead :: TreacheryCard FalseLead
falseLead = treachery FalseLead Cards.falseLead

-- | When Revealed: The first player chooses and shuffles a card with the printed Clue
-- trait back into the encounter deck. If there are no Clue cards in play, False Lead gains
-- surge.
instance RunMessage FalseLead where
  runMessage msg t@(FalseLead attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      clues <- objectivesWithTrait Clue
      lead <- getLead
      if null clues
        then revealEncounterCard
        else chooseOrRunOneM lead do
          questionLabeled "falseLead.shuffle"
          causedBy attrs
          targets clues shuffleIntoEncounterDeck
      pure t
    _ -> FalseLead <$> runMessage msg attrs
