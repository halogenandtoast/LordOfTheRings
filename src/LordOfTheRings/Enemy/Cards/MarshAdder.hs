module LordOfTheRings.Enemy.Cards.MarshAdder (marshAdder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MarshAdder = MarshAdder EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

marshAdder :: EnemyCard MarshAdder
marshAdder = enemy MarshAdder Cards.marshAdder
