module LordOfTheRings.Objective.Cards.TheOneRingTheRoadDarkens (theOneRingTheRoadDarkens) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype TheOneRingTheRoadDarkens = TheOneRingTheRoadDarkens ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOneRingTheRoadDarkens :: ObjectiveCard TheOneRingTheRoadDarkens
theOneRingTheRoadDarkens = objective TheOneRingTheRoadDarkens Cards.theOneRingTheRoadDarkens
