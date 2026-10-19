module LordOfTheRings.Ally.Cards.GuardOfTheCitadel (guardOfTheCitadel) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GuardOfTheCitadel = GuardOfTheCitadel AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

guardOfTheCitadel :: AllyCard GuardOfTheCitadel
guardOfTheCitadel = ally GuardOfTheCitadel Cards.guardOfTheCitadel
