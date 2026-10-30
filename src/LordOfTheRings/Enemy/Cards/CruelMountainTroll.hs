module LordOfTheRings.Enemy.Cards.CruelMountainTroll (cruelMountainTroll) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CruelMountainTroll = CruelMountainTroll EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

cruelMountainTroll :: EnemyCard CruelMountainTroll
cruelMountainTroll = enemy CruelMountainTroll Cards.cruelMountainTroll

-- | For each excess point of combat damage dealt by Cruel Mountain-troll (damage that is
-- dealt beyond the remaining hit points of the character damaged by its attack) you must
-- remove 1 progress from each quest card in play.
instance HasModifiersFor CruelMountainTroll where
  getModifiersFor (CruelMountainTroll a) = modified a a [ExcessDamageRemovesQuestProgress]
