module LordOfTheRings.Ally.Cards.WardenOfHelmsDeep (wardenOfHelmsDeep) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype WardenOfHelmsDeep = WardenOfHelmsDeep AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wardenOfHelmsDeep :: AllyCard WardenOfHelmsDeep
wardenOfHelmsDeep = ally WardenOfHelmsDeep Cards.wardenOfHelmsDeep
