module LordOfTheRings.Enemy.Cards.OrcGuard (orcGuard) where

import LordOfTheRings.Card.Synthetic qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

-- | A player card fighting face down as an enemy, with no text of its own.
newtype OrcGuard = OrcGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcGuard :: EnemyCard OrcGuard
orcGuard = enemy OrcGuard Cards.orcGuard
