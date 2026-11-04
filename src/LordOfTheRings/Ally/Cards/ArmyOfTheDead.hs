module LordOfTheRings.Ally.Cards.ArmyOfTheDead (armyOfTheDead) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype ArmyOfTheDead = ArmyOfTheDead AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The boon side of Army of the Dead (The Passing of the Grey Company's resolution). The
-- first player gains control of Army of the Dead: its keyword.
armyOfTheDead :: AllyCard ArmyOfTheDead
armyOfTheDead = ally ArmyOfTheDead Cards.armyOfTheDeadBoon

-- | Immune to player card effects. If Army of the Dead leaves play, remove it from the game.
instance HasModifiersFor ArmyOfTheDead where
  getModifiersFor (ArmyOfTheDead a) = modifySelf a [ImmuneToPlayerCardEffects, RemoveFromGameIfLeavesPlay]

instance RunMessage ArmyOfTheDead where
  runMessage msg (ArmyOfTheDead a) = ArmyOfTheDead <$> runMessage msg a
