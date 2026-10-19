module LordOfTheRings.Enemy.Cards.NazgulOfDolGuldur (nazgulOfDolGuldur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype NazgulOfDolGuldur = NazgulOfDolGuldur EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

nazgulOfDolGuldur :: EnemyCard NazgulOfDolGuldur
nazgulOfDolGuldur = enemy NazgulOfDolGuldur Cards.nazgulOfDolGuldur
