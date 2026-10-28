module LordOfTheRings.Treachery.Cards.ImpenetrableFog (impenetrableFog) where

import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ImpenetrableFog = ImpenetrableFog TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

impenetrableFog :: TreacheryCard ImpenetrableFog
impenetrableFog = treachery ImpenetrableFog Cards.impenetrableFog

-- | When Revealed: The first player either places 3 progress tokens on To the Tower or
-- reveals X additional cards from the encounter deck and adds them to the staging area. X
-- is the number of players in the game.
instance RunMessage ImpenetrableFog where
  runMessage msg t@(ImpenetrableFog attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      players <- length <$> getRemainingPlayers
      chooseOneM lead do
        questionLabeled "impenetrableFog.choose"
        causedBy attrs
        labeled "impenetrableFog.progress" $ placeOnTower attrs 3
        labeledWith "impenetrableFog.reveal" [intVar "count" players] $ revealEncounterCards players
      pure t
    _ -> ImpenetrableFog <$> runMessage msg attrs
