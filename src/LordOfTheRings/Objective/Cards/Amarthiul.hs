module LordOfTheRings.Objective.Cards.Amarthiul (amarthiul) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Amarthiul = Amarthiul ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Amarthiúl, as an ally, as he comes out.
amarthiul :: ObjectiveCard Amarthiul
amarthiul = objective Amarthiul Cards.amarthiul
