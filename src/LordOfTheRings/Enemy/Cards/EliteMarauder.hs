module LordOfTheRings.Enemy.Cards.EliteMarauder (eliteMarauder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EliteMarauder = EliteMarauder EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eliteMarauder :: EnemyCard EliteMarauder
eliteMarauder = enemy EliteMarauder Cards.eliteMarauder

-- | For each excess point of damage dealt by Elite Marauder (damage that is dealt beyond
-- the remaining hit points of the character damaged by its attack) you must place 1 damage
-- on an Aflame location in the staging area.
instance HasModifiersFor EliteMarauder where
  getModifiersFor (EliteMarauder a) = modifySelf a [ExcessDamageToAflameLocation]
