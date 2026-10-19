module LordOfTheRings.Enemy.Cards.Hummerhorns (hummerhorns) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Hummerhorns = Hummerhorns EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hummerhorns :: EnemyCard Hummerhorns
hummerhorns = enemy Hummerhorns Cards.hummerhorns
