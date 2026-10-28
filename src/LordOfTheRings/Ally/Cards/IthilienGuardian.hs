module LordOfTheRings.Ally.Cards.IthilienGuardian (ithilienGuardian) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype IthilienGuardian = IthilienGuardian AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, once The Hidden Way gives him to the first player.
ithilienGuardian :: AllyCard IthilienGuardian
ithilienGuardian = ally IthilienGuardian Cards.ithilienGuardian
