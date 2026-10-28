module LordOfTheRings.Objective.Cards.FaramirObjective (faramirObjective) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype FaramirObjective = FaramirObjective ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Faramir, as an ally, as he comes out.
faramirObjective :: ObjectiveCard FaramirObjective
faramirObjective = objective FaramirObjective Cards.faramirObjective
