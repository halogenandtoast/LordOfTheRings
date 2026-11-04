module LordOfTheRings.Enemy.Cards.HiveGuardian (hiveGuardian) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (isOnTrack)

newtype HiveGuardian = HiveGuardian EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hiveGuardian :: EnemyCard HiveGuardian
hiveGuardian = enemy HiveGuardian Cards.hiveGuardian

-- | While On Track is in play, Hive Guardian gets +2 attack and -5 engagement cost.
instance HasModifiersFor HiveGuardian where
  getModifiersFor (HiveGuardian a) = whenM isOnTrack $ modifySelf a [AttackModifier 2, EngagementCostModifier (-5)]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if On Track is in play).
instance RunMessage HiveGuardian where
  runMessage msg e@(HiveGuardian attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      on <- isOnTrack
      enemyGetsAttack attrs eid (if on then 2 else 1)
      pure e
    _ -> HiveGuardian <$> runMessage msg attrs
