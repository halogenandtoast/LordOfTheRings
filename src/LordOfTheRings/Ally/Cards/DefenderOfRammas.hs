module LordOfTheRings.Ally.Cards.DefenderOfRammas (defenderOfRammas) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DefenderOfRammas = DefenderOfRammas AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

defenderOfRammas :: AllyCard DefenderOfRammas
defenderOfRammas = ally DefenderOfRammas Cards.defenderOfRammas
