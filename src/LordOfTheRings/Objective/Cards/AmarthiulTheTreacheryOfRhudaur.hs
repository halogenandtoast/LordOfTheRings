module LordOfTheRings.Objective.Cards.AmarthiulTheTreacheryOfRhudaur (amarthiulTheTreacheryOfRhudaur) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype AmarthiulTheTreacheryOfRhudaur = AmarthiulTheTreacheryOfRhudaur ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Amarthiúl, as an ally, as he comes out.
amarthiulTheTreacheryOfRhudaur :: ObjectiveCard AmarthiulTheTreacheryOfRhudaur
amarthiulTheTreacheryOfRhudaur = objective AmarthiulTheTreacheryOfRhudaur Cards.amarthiulTheTreacheryOfRhudaur
