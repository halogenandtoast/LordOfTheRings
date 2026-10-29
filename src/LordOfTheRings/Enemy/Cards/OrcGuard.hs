module LordOfTheRings.Enemy.Cards.OrcGuard (orcGuard) where

import LordOfTheRings.Card.Synthetic qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

-- | A player card fighting face down as an enemy, with no text of its own.
newtype OrcGuard = OrcGuard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcGuard :: EnemyCard OrcGuard
orcGuard = enemy OrcGuard Cards.orcGuard

-- | It never reaches the staging area: the game discards it instead (FAQ errata).
instance RunMessage OrcGuard where
  runMessage msg e@(OrcGuard attrs) = case msg of
    PlaceEnemy eid StagingArea | eid == attrs.id -> pure e
    _ -> OrcGuard <$> runMessage msg attrs
