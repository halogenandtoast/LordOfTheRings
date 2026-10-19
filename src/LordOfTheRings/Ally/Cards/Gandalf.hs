module LordOfTheRings.Ally.Cards.Gandalf (gandalf) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Gandalf = Gandalf AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gandalf :: AllyCard Gandalf
gandalf = ally Gandalf Cards.gandalf
