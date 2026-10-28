module LordOfTheRings.Ally.Cards.WarriorOfLossarnach (warriorOfLossarnach) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Trait (Trait (Outlands))

newtype WarriorOfLossarnach = WarriorOfLossarnach AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

warriorOfLossarnach :: AllyCard WarriorOfLossarnach
warriorOfLossarnach = ally WarriorOfLossarnach Cards.warriorOfLossarnach

-- | Each Outlands character you control gets +1 defense.
instance HasModifiersFor WarriorOfLossarnach where
  getModifiersFor (WarriorOfLossarnach a) = for_ (placementController a.placement) \pid -> do
    outlands <- controlledCharactersWithTrait pid Outlands
    for_ outlands \c -> modified a c [DefenseModifier 1]
