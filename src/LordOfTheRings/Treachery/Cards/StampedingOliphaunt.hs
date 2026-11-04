module LordOfTheRings.Treachery.Cards.StampedingOliphaunt (stampedingOliphaunt) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (wildMumak)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype StampedingOliphaunt = StampedingOliphaunt TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1, on its card def.
stampedingOliphaunt :: TreacheryCard StampedingOliphaunt
stampedingOliphaunt = treachery StampedingOliphaunt Cards.stampedingOliphaunt

-- | When Revealed: Choose a Wild Mûmak in the staging area to attack the first player. If
-- no attacks are made this way, search the encounter deck and discard pile for a Mûmak
-- Trail and add it to the staging area. Shuffle the encounter deck. (Hall of Beorn's text:
-- cards.json has only the keyword.) The first player chooses.
instance RunMessage StampedingOliphaunt where
  runMessage msg t@(StampedingOliphaunt attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      mumaks <- select (wildMumak <> EnemyInStagingArea)
      if null mumaks
        then searchEncounterDeckAndDiscard lead attrs (cardIs Locations.mumakTrail)
        else chooseOrRunOneM lead do
          questionLabeled "stampedingOliphaunt.attack"
          causedBy attrs
          targets mumaks $ immediateAttack lead
      pure t
    _ -> StampedingOliphaunt <$> runMessage msg attrs
