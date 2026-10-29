module LordOfTheRings.Treachery.Cards.RaisingTheCry (raisingTheCry) where

import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (removeTimeCounterFromEachLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RaisingTheCry = RaisingTheCry TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

raisingTheCry :: TreacheryCard RaisingTheCry
raisingTheCry = treachery RaisingTheCry Cards.raisingTheCry

-- | When Revealed: Remove 1 time counter from each location in play. Place X time counters
-- on each location in play with no time counters on it. X is the "Time X" value on that
-- location. If there are no locations in the staging area, Raising the Cry gains surge.
-- The counters are placed once the removals, and the Forced effects they set off, are done.
instance RunMessage RaisingTheCry where
  runMessage msg t@(RaisingTheCry attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      removeTimeCounterFromEachLocation attrs
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      locations <- select AnyLocation
      for_ locations \l -> do
        counters <- getTimeCounters (toTarget l)
        x <- getTimeX (toTarget l)
        when (counters == 0 && x > 0) $ placeTimeCounters attrs l x
      whenM (selectNone LocationInStagingArea) revealEncounterCard
      pure t
    _ -> RaisingTheCry <$> runMessage msg attrs
