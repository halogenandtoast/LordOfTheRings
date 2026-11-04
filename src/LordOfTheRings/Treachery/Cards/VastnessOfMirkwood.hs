module LordOfTheRings.Treachery.Cards.VastnessOfMirkwood (vastnessOfMirkwood) where

import LordOfTheRings.Scenario.Scenarios.LostInMirkwood.Helpers (objectiveLocationsInPlay)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype VastnessOfMirkwood = VastnessOfMirkwood TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

vastnessOfMirkwood :: TreacheryCard VastnessOfMirkwood
vastnessOfMirkwood = treachery VastnessOfMirkwood Cards.vastnessOfMirkwood

{- | When Revealed: Until the end of the phase, each non-objective location in the staging
area gets +1 threat (+2 threat instead if it has at least 1 progress token on it). If the
number of non-objective locations in the staging area is less than the number of players,
Vastness of Mirkwood gains surge. The locations are those in the staging area as it
resolves, each read for progress then.
-}
instance RunMessage VastnessOfMirkwood where
  runMessage msg t@(VastnessOfMirkwood attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      objectives <- objectiveLocationsInPlay
      staged <- filter (`notElem` objectives) <$> select LocationInStagingArea
      for_ staged \lid -> do
        l <- getLocation lid
        phaseModifier attrs lid (ThreatModifier $ if l.progress > 0 then 2 else 1)
      players <- length <$> getRemainingPlayers
      when (length staged < players) revealEncounterCard
      pure t
    _ -> VastnessOfMirkwood <$> runMessage msg attrs
