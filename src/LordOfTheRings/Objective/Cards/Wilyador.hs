module LordOfTheRings.Objective.Cards.Wilyador (wilyador) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Wilyador = Wilyador ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Wilyador, as an ally, as he comes out.
wilyador :: ObjectiveCard Wilyador
wilyador = objective Wilyador Cards.wilyador
