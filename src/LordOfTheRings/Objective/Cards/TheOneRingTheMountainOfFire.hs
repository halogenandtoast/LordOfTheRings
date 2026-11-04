module LordOfTheRings.Objective.Cards.TheOneRingTheMountainOfFire (theOneRingTheMountainOfFire) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype TheOneRingTheMountainOfFire = TheOneRingTheMountainOfFire ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Mountain of Fire's The One Ring, attached to the Ring-bearer as the game is set up.
-- Its text is The Mountain of Fire player cards' to give it.
theOneRingTheMountainOfFire :: ObjectiveCard TheOneRingTheMountainOfFire
theOneRingTheMountainOfFire = objective TheOneRingTheMountainOfFire Cards.theOneRingTheMountainOfFire

instance RunMessage TheOneRingTheMountainOfFire where
  runMessage msg (TheOneRingTheMountainOfFire attrs) = TheOneRingTheMountainOfFire <$> runMessage msg attrs
