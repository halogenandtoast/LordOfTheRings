module LordOfTheRings.Ally.Cards.GrimbeornTheOld (grimbeornTheOld) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Trait (Trait (Troll))

newtype GrimbeornTheOld = GrimbeornTheOld AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally, once he has joined the first player.
grimbeornTheOld :: AllyCard GrimbeornTheOld
grimbeornTheOld = ally GrimbeornTheOld Cards.grimbeornTheOld

-- | Grimbeorn the Old does not exhaust to defend against Troll enemies.
instance HasModifiersFor GrimbeornTheOld where
  getModifiersFor (GrimbeornTheOld a) = modified a a [DoesNotExhaustToDefendAgainst Troll]

-- | "He joins the first player as an ally" is a passive effect: he follows the first
-- player token as it passes.
instance RunMessage GrimbeornTheOld where
  runMessage msg (GrimbeornTheOld a) = case msg of
    SetFirstPlayer pid | a.placement /= InPlayArea pid -> do
      takeControl a.id pid
      GrimbeornTheOld <$> runMessage msg a
    _ -> GrimbeornTheOld <$> runMessage msg a
