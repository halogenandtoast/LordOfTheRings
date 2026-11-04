module LordOfTheRings.Location.Cards.RiverRunning (riverRunning) where

import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype RiverRunning = RiverRunning LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

riverRunning :: LocationCard RiverRunning
riverRunning = location RiverRunning Cards.riverRunning

-- | While River Running is in the staging area, enemies cannot be optionally engaged.
instance HasModifiersFor RiverRunning where
  getModifiersFor (RiverRunning a) =
    when (a.placement == StagingArea) $ modified a GameTarget [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Travel: The first player engages the highest engagement cost enemy in the staging
-- area, choosing among those tied.
instance RunMessage RiverRunning where
  runMessage msg l@(RiverRunning a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      enemies <- traverse (\e -> (e,) <$> getEngagementCost e) =<< select EnemyInStagingArea
      for_ (maximumMay $ map snd enemies) \highest ->
        chooseOrRunOneM lead do
          questionLabeled "riverRunning.engage"
          causedBy a
          targets [e | (e, cost) <- enemies, cost == highest] (engageEnemy lead)
      pure l
    _ -> RiverRunning <$> runMessage msg a
