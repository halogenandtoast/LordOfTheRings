module LordOfTheRings.Enemy.Cards.GoblinSniper (goblinSniper) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinSniper = GoblinSniper EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinSniper :: EnemyCard GoblinSniper
goblinSniper = enemy GoblinSniper Cards.goblinSniper
