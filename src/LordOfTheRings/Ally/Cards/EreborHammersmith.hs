module LordOfTheRings.Ally.Cards.EreborHammersmith (ereborHammersmith) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype EreborHammersmith = EreborHammersmith AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborHammersmith :: AllyCard EreborHammersmith
ereborHammersmith = ally EreborHammersmith Cards.ereborHammersmith
