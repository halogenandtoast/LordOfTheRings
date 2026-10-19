module LordOfTheRings.Ally.Cards.BrokIronfist (brokIronfist) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype BrokIronfist = BrokIronfist AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

brokIronfist :: AllyCard BrokIronfist
brokIronfist = ally BrokIronfist Cards.brokIronfist
