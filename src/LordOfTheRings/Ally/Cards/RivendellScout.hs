module LordOfTheRings.Ally.Cards.RivendellScout (rivendellScout) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype RivendellScout = RivendellScout AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

rivendellScout :: AllyCard RivendellScout
rivendellScout = ally RivendellScout Cards.rivendellScout
