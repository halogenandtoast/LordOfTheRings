module LordOfTheRings.Ally.Cards.GrimbeornTheOld (grimbeornTheOld) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Trait (Trait (Troll))

newtype GrimbeornTheOld = GrimbeornTheOld AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, once he has joined the first player.
grimbeornTheOld :: AllyCard GrimbeornTheOld
grimbeornTheOld = ally GrimbeornTheOld Cards.grimbeornTheOld

-- | Grimbeorn the Old does not exhaust to defend against Troll enemies.
instance HasModifiersFor GrimbeornTheOld where
  getModifiersFor (GrimbeornTheOld a) = modified a a [DoesNotExhaustToDefendAgainst Troll]
