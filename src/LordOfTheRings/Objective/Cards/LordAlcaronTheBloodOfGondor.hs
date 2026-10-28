module LordOfTheRings.Objective.Cards.LordAlcaronTheBloodOfGondor (lordAlcaronTheBloodOfGondor) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype LordAlcaronTheBloodOfGondor = LordAlcaronTheBloodOfGondor ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Lord Alcaron, as an ally, as he comes out.
lordAlcaronTheBloodOfGondor :: ObjectiveCard LordAlcaronTheBloodOfGondor
lordAlcaronTheBloodOfGondor = objective LordAlcaronTheBloodOfGondor Cards.lordAlcaronTheBloodOfGondor
