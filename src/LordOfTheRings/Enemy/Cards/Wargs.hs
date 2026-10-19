module LordOfTheRings.Enemy.Cards.Wargs (wargs) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Wargs = Wargs EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wargs :: EnemyCard Wargs
wargs = enemy Wargs Cards.wargs
