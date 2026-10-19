module LordOfTheRings.Ally.Cards.SilverlodeArcher (silverlodeArcher) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype SilverlodeArcher = SilverlodeArcher AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

silverlodeArcher :: AllyCard SilverlodeArcher
silverlodeArcher = ally SilverlodeArcher Cards.silverlodeArcher
