module LordOfTheRings.Objective.Cards.Calphon (calphon) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Calphon = Calphon ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Calphon, as an ally, as he comes out.
calphon :: ObjectiveCard Calphon
calphon = objective Calphon Cards.calphon
