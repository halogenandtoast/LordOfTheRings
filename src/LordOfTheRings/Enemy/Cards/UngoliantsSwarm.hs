module LordOfTheRings.Enemy.Cards.UngoliantsSwarm (ungoliantsSwarm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UngoliantsSwarm = UngoliantsSwarm EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ungoliantsSwarm :: EnemyCard UngoliantsSwarm
ungoliantsSwarm = enemy UngoliantsSwarm Cards.ungoliantsSwarm
