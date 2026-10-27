module LordOfTheRings.Objective.Cards.ArwenUndomielObjective (
  arwenUndomielTheRedhornGate,
  arwenUndomielRoadToRivendell,
) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype ArwenUndomiel = ArwenUndomiel ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Arwen Undómiel, as an ally, as she comes out.
arwenUndomielTheRedhornGate :: ObjectiveCard ArwenUndomiel
arwenUndomielTheRedhornGate = objective ArwenUndomiel Cards.arwenUndomielTheRedhornGate

arwenUndomielRoadToRivendell :: ObjectiveCard ArwenUndomiel
arwenUndomielRoadToRivendell = objective ArwenUndomiel Cards.arwenUndomielRoadToRivendell
