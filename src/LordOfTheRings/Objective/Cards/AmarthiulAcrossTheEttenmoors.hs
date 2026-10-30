module LordOfTheRings.Objective.Cards.AmarthiulAcrossTheEttenmoors (amarthiulAcrossTheEttenmoors) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype AmarthiulAcrossTheEttenmoors = AmarthiulAcrossTheEttenmoors ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Amarthiúl, as an ally, as he comes out.
amarthiulAcrossTheEttenmoors :: ObjectiveCard AmarthiulAcrossTheEttenmoors
amarthiulAcrossTheEttenmoors = objective AmarthiulAcrossTheEttenmoors Cards.amarthiulAcrossTheEttenmoors
