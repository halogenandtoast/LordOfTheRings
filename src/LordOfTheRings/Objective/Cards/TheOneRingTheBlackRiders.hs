module LordOfTheRings.Objective.Cards.TheOneRingTheBlackRiders (theOneRingTheBlackRiders) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype TheOneRingTheBlackRiders = TheOneRingTheBlackRiders ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Black Riders' The One Ring, attached to the Ring-bearer as its scenarios are set up.
-- Its text is The Black Riders' player cards' to give.
theOneRingTheBlackRiders :: ObjectiveCard TheOneRingTheBlackRiders
theOneRingTheBlackRiders = objective TheOneRingTheBlackRiders Cards.theOneRingTheBlackRiders

instance RunMessage TheOneRingTheBlackRiders where
  runMessage msg (TheOneRingTheBlackRiders a) = TheOneRingTheBlackRiders <$> runMessage msg a
