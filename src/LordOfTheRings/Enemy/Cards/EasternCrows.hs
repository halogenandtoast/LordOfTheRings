module LordOfTheRings.Enemy.Cards.EasternCrows (easternCrows) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype EasternCrows = EasternCrows EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternCrows :: EnemyCard EasternCrows
easternCrows = enemy EasternCrows Cards.easternCrows
