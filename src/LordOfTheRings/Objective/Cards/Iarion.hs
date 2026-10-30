module LordOfTheRings.Objective.Cards.Iarion (iarion) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Iarion = Iarion ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Iârion, as an ally, as he comes out.
iarion :: ObjectiveCard Iarion
iarion = objective Iarion Cards.iarion
