module LordOfTheRings.Enemy.Cards.CavernGuardian (cavernGuardian) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CavernGuardian = CavernGuardian EnemyAttrs
  deriving anyclass (IsEnemy, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

cavernGuardian :: EnemyCard CavernGuardian
cavernGuardian = enemy CavernGuardian Cards.cavernGuardian
