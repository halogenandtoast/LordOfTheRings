module LordOfTheRings.Objective.Cards.Abaan (abaan) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype Abaan = Abaan ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The objective ally: in play he is an ally (Ally.Cards.Abaan), the captive of Dungeon
-- Cell from the start.
abaan :: ObjectiveCard Abaan
abaan = objective Abaan Cards.abaan
