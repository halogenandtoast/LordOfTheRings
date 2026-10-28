module LordOfTheRings.Objective.Cards.LordAlcaron (lordAlcaron) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype LordAlcaron = LordAlcaron ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Lord Alcaron, as an ally, as he comes out.
lordAlcaron :: ObjectiveCard LordAlcaron
lordAlcaron = objective LordAlcaron Cards.lordAlcaron
