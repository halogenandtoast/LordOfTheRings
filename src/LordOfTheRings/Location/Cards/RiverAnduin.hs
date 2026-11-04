module LordOfTheRings.Location.Cards.RiverAnduin (riverAnduin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (playersAtThisStage, stagingAreaToTheLeft)

newtype RiverAnduin = RiverAnduin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

riverAnduin :: LocationCard RiverAnduin
riverAnduin = location RiverAnduin Cards.riverAnduin

{- | While River Anduin is in the staging area it gains: "Forced: At the end of the travel
phase, each player at this stage raises his threat by 1. Then, River Anduin moves to the
staging area to the left, if able." The end of the phase is one moment at every stage: one
that moved to an area still to see it end does not move on again.
-}
instance HasAbilities RiverAnduin where
  getAbilities (RiverAnduin a) =
    [limited (GroupLimit PerPhase 1) $ mkAbility a 1 $ forced $ PhaseEndsWindow When TravelPhase | a.placement == StagingArea]

instance RunMessage RiverAnduin where
  runMessage msg l@(RiverAnduin a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- playersAtThisStage
      for_ players \pid -> encounterRaisesThreat pid 1
      traverse_ (`placeInStagingAreaOf` a) =<< stagingAreaToTheLeft
      pure l
    _ -> RiverAnduin <$> runMessage msg a
