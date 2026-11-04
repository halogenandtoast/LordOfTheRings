module LordOfTheRings.Objective.Cards.TheBlackGate (theBlackGate) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (haradEnemy, placeUnderTheBlackGate)

newtype TheBlackGate = TheBlackGate ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBlackGate :: ObjectiveCard TheBlackGate
theBlackGate = objective TheBlackGate Cards.theBlackGate

-- | Forced: At the end of the planning phase, place each Harad enemy in the staging area
-- facedown under The Black Gate.
instance HasAbilities TheBlackGate where
  getAbilities (TheBlackGate a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When PlanningPhase]

{- | If there are at least X facedown cards under The Black Gate, the players lose the game.
X is 3 more than the number of players: checked as cards go under it, and as a player is
eliminated.
-}
instance RunMessage TheBlackGate where
  runMessage msg o@(TheBlackGate attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      traverse_ placeUnderTheBlackGate =<< select (haradEnemy <> EnemyInStagingArea)
      pure o
    PlaceUnderneath (isTarget attrs -> True) _ -> do
      checkGate
      TheBlackGate <$> runMessage msg attrs
    EliminatePlayer _ -> do
      checkGate
      TheBlackGate <$> runMessage msg attrs
    ScenarioSpecific "theBlackGate.check" _ -> do
      players <- length <$> getRemainingPlayers
      when (length attrs.objectiveUnderneath >= players + 3) $ gameOver Lost
      pure o
    _ -> TheBlackGate <$> runMessage msg attrs
   where
    checkGate = push $ ScenarioSpecific "theBlackGate.check" Null
