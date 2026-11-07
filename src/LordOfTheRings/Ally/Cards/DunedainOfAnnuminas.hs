module LordOfTheRings.Ally.Cards.DunedainOfAnnuminas (dunedainOfAnnuminas) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype DunedainOfAnnuminas = DunedainOfAnnuminas AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainOfAnnuminas :: AllyCard DunedainOfAnnuminas
dunedainOfAnnuminas = ally DunedainOfAnnuminas Cards.dunedainOfAnnuminas
