module LordOfTheRings.Ally.Cards.Gleowine (gleowine) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Gleowine = Gleowine AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gleowine :: AllyCard Gleowine
gleowine = ally Gleowine Cards.gleowine
