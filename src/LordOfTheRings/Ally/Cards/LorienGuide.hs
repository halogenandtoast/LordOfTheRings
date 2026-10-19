module LordOfTheRings.Ally.Cards.LorienGuide (lorienGuide) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype LorienGuide = LorienGuide AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

lorienGuide :: AllyCard LorienGuide
lorienGuide = ally LorienGuide Cards.lorienGuide
