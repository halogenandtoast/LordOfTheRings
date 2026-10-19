module LordOfTheRings.Enemy.Cards.ForestSpider (forestSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ForestSpider = ForestSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestSpider :: EnemyCard ForestSpider
forestSpider = enemy ForestSpider Cards.forestSpider
