module LordOfTheRings.Ally.Cards.Faramir (faramir) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Faramir = Faramir AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

faramir :: AllyCard Faramir
faramir = ally Faramir Cards.faramir
