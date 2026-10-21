module LordOfTheRings.Enemy.Cards.OrcHorseThieves (orcHorseThieves) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcHorseThieves = OrcHorseThieves EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcHorseThieves :: EnemyCard OrcHorseThieves
orcHorseThieves = enemy OrcHorseThieves Cards.orcHorseThieves

-- | Orc Horse Thieves get +1 attack for each location in the staging area.
instance HasModifiersFor OrcHorseThieves where
  getModifiersFor (OrcHorseThieves a) = do
    locations <- selectCount LocationInStagingArea
    when (locations > 0) $ modified a a [AttackModifier locations]
