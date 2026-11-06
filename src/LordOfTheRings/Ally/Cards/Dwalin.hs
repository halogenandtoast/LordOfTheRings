module LordOfTheRings.Ally.Cards.Dwalin (dwalin) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype Dwalin = Dwalin AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Sentinel, on its card def.
dwalin :: AllyCard Dwalin
dwalin = ally Dwalin Cards.dwalin

-- | While you control at least 5 Dwarf characters, lower the cost to play Dwalin by 2.
instance HasModifiersFor Dwalin where
  getModifiersFor (Dwalin a) = do
    dwarves <- dwarfCount a.controller
    when (dwarves >= 5) $ modified a (CardIdTarget a.allyCardId) [CostModifier (-2)]
