module LordOfTheRings.Enemy.Cards.UngoliantsSpawn (ungoliantsSpawn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UngoliantsSpawn = UngoliantsSpawn EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ungoliantsSpawn :: EnemyCard UngoliantsSpawn
ungoliantsSpawn = enemy UngoliantsSpawn Cards.ungoliantsSpawn
