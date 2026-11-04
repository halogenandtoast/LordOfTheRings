module LordOfTheRings.Objective.Cards.TheWainriders (theWainriders) where

import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (placeWainridersDamage)

{- | The Wainriders' position in the race, at a stage of the circuit rather than in the
staging area: the scenario keeps which stage, and the damage on it. "If The Wainriders'
stage has damage on it equal to or greater than its quest points, The Wainriders advances
to the next stage in the circuit. Place any excess damage on that stage" is the
scenario's, as damage is placed.
-}
newtype TheWainriders = TheWainriders ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWainriders :: ObjectiveCard TheWainriders
theWainriders = objective TheWainriders Cards.theWainriders

-- | Forced: At the end of the quest phase, place X damage on The Wainriders' stage, where X
-- is the total Threat of each enemy in the staging area.
instance HasAbilities TheWainriders where
  getAbilities (TheWainriders a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase]

instance RunMessage TheWainriders where
  runMessage msg o@(TheWainriders attrs) = case msg of
    ScenarioSpecific "circuit.placeObjectives" _ -> do
      placeObjective attrs.objectiveId Unplaced
      pure o
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- sum <$> (traverse getEnemyThreat =<< select EnemyInStagingArea)
      placeWainridersDamage x
      pure o
    _ -> TheWainriders <$> runMessage msg attrs
