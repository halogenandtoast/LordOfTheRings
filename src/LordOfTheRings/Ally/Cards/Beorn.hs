module LordOfTheRings.Ally.Cards.Beorn (beorn) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Beorn = Beorn AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

beorn :: AllyCard Beorn
beorn = ally Beorn Cards.beorn
