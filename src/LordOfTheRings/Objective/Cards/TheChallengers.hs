module LordOfTheRings.Objective.Cards.TheChallengers (theChallengers) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

{- | The players' position in the race, at a stage of the circuit rather than in the
staging area: the scenario keeps which stage, the current quest. "If The Challengers
completes two circuits before the Wainriders, the players win the game. If The Wainriders
completes two circuits before the challengers, the players lose the game" is the
scenario's, as either advances.
-}
newtype TheChallengers = TheChallengers ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theChallengers :: ObjectiveCard TheChallengers
theChallengers = objective TheChallengers Cards.theChallengers

-- | Forced: When the players defeat a quest stage with the race keyword, advance this card
-- to the next quest stage in the circuit.
instance RunMessage TheChallengers where
  runMessage msg o@(TheChallengers attrs) = case msg of
    ScenarioSpecific "circuit.placeObjectives" _ -> do
      placeObjective attrs.objectiveId Unplaced
      pure o
    ScenarioSpecific "race.defeated" excess -> do
      push $ ScenarioSpecific "challengers.advance" excess
      pure o
    _ -> TheChallengers <$> runMessage msg attrs
