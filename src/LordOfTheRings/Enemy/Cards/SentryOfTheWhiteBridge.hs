module LordOfTheRings.Enemy.Cards.SentryOfTheWhiteBridge (sentryOfTheWhiteBridge) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower, towerProgress)

newtype SentryOfTheWhiteBridge = SentryOfTheWhiteBridge EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

sentryOfTheWhiteBridge :: EnemyCard SentryOfTheWhiteBridge
sentryOfTheWhiteBridge = enemy SentryOfTheWhiteBridge Cards.sentryOfTheWhiteBridge

-- | X is the number of progress tokens on To the Tower.
instance HasModifiersFor SentryOfTheWhiteBridge where
  getModifiersFor (SentryOfTheWhiteBridge a) = do
    x <- towerProgress
    when (x > 0) $ modified a a [ThreatModifier x, AttackModifier x]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, place 1
-- progress on To the Tower.
instance RunMessage SentryOfTheWhiteBridge where
  runMessage msg e@(SentryOfTheWhiteBridge attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeOnTower attrs 1
      pure e
    _ -> SentryOfTheWhiteBridge <$> runMessage msg attrs
