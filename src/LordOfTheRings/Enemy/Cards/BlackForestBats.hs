module LordOfTheRings.Enemy.Cards.BlackForestBats (blackForestBats) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype BlackForestBats = BlackForestBats EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

blackForestBats :: EnemyCard BlackForestBats
blackForestBats = enemy BlackForestBats Cards.blackForestBats
