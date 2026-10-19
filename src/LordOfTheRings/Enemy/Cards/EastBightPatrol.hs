module LordOfTheRings.Enemy.Cards.EastBightPatrol (eastBightPatrol) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EastBightPatrol = EastBightPatrol EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastBightPatrol :: EnemyCard EastBightPatrol
eastBightPatrol = enemy EastBightPatrol Cards.eastBightPatrol
