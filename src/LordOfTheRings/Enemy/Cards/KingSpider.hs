module LordOfTheRings.Enemy.Cards.KingSpider (kingSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype KingSpider = KingSpider EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

kingSpider :: EnemyCard KingSpider
kingSpider = enemy KingSpider Cards.kingSpider
