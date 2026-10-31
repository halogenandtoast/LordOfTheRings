module LordOfTheRings.Enemy.Cards.DruadanStalker (druadanStalker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (prowlValue)

newtype DruadanStalker = DruadanStalker EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

druadanStalker :: EnemyCard DruadanStalker
druadanStalker = enemy DruadanStalker Cards.druadanStalker

-- | Archery 3.
-- As an additional cost to optionally engage Drúadan Stalker, you must spend 2 resources.
instance HasModifiersFor DruadanStalker where
  getModifiersFor (DruadanStalker a) = modified a a [OptionalEngagementCost 2]

-- | Shadow: Attacking enemy gets +X attack. X is that enemy's prowl value.
instance RunMessage DruadanStalker where
  runMessage msg e@(DruadanStalker attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- prowlValue eid
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure e
    _ -> DruadanStalker <$> runMessage msg attrs
