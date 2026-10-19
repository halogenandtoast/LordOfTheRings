module LordOfTheRings.Ally.Cards.LongbeardOrcSlayer (longbeardOrcSlayer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype LongbeardOrcSlayer = LongbeardOrcSlayer AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

longbeardOrcSlayer :: AllyCard LongbeardOrcSlayer
longbeardOrcSlayer = ally LongbeardOrcSlayer Cards.longbeardOrcSlayer
