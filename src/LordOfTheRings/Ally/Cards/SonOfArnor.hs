module LordOfTheRings.Ally.Cards.SonOfArnor (sonOfArnor) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype SonOfArnor = SonOfArnor AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

sonOfArnor :: AllyCard SonOfArnor
sonOfArnor = ally SonOfArnor Cards.sonOfArnor
