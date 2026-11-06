module LordOfTheRings.Ally.Cards.RangerOfIthilien (rangerOfIthilien) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype RangerOfIthilien = RangerOfIthilien AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- Its revelation and shadow are resolved as an objective; once controlled it is
-- an ordinary objective ally, with no continuing ability.
rangerOfIthilien :: AllyCard RangerOfIthilien
rangerOfIthilien = ally RangerOfIthilien Cards.rangerOfIthilien
