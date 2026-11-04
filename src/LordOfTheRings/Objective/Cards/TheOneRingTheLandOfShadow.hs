module LordOfTheRings.Objective.Cards.TheOneRingTheLandOfShadow (theOneRingTheLandOfShadow) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

-- | The Land of Shadow's One Ring, attached to the Ring-bearer as the scenario is set up.
newtype TheOneRingTheLandOfShadow = TheOneRingTheLandOfShadow ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOneRingTheLandOfShadow :: ObjectiveCard TheOneRingTheLandOfShadow
theOneRingTheLandOfShadow = objective TheOneRingTheLandOfShadow Cards.theOneRingTheLandOfShadow
