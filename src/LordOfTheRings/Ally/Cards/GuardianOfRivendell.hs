module LordOfTheRings.Ally.Cards.GuardianOfRivendell (guardianOfRivendell) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GuardianOfRivendell = GuardianOfRivendell AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | As an additional cost to play Guardian of Rivendell from your hand, you must discard 2
-- cards from your hand: on its card def.
guardianOfRivendell :: AllyCard GuardianOfRivendell
guardianOfRivendell = ally GuardianOfRivendell Cards.guardianOfRivendell
