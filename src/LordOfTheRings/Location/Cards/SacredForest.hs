module LordOfTheRings.Location.Cards.SacredForest (sacredForest) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Guardian))

newtype SacredForest = SacredForest LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

sacredForest :: LocationCard SacredForest
sacredForest = location SacredForest Cards.sacredForest

-- | While Sacred Forest is in the staging area, each Guardian enemy gains: "Forced: After
-- any number of time counters are removed from this enemy, heal 2 damage on it for each
-- time counter removed."
-- Travel: Remove each time counter from an enemy in play to travel here.
instance RunMessage SacredForest where
  runMessage msg l@(SacredForest a) = case msg of
    CheckWindows ws | a.placement == StagingArea -> do
      guardians <- select (EnemyWithTrait Guardian)
      for_ [(eid, n) | Window After (TimeCountersRemoved (EnemyTarget eid) n) <- ws, eid `elem` guardians] \(eid, n) ->
        healDamage a eid (2 * n)
      SacredForest <$> runMessage msg a
    TravelTo lid | lid == a.id -> do
      timed <- filterM (\e -> (> 0) <$> getTimeCounters (toTarget e)) =<< select AnyEnemy
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "sacredForest.removeTimeCounters"
        causedBy a
        targets timed \e -> removeTimeCounters a e =<< getTimeCounters (toTarget e)
      pure l
    _ -> SacredForest <$> runMessage msg a
