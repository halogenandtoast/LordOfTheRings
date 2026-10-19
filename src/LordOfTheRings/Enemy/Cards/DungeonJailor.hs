module LordOfTheRings.Enemy.Cards.DungeonJailor (dungeonJailor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DungeonJailor = DungeonJailor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dungeonJailor :: EnemyCard DungeonJailor
dungeonJailor = enemy DungeonJailor Cards.dungeonJailor
