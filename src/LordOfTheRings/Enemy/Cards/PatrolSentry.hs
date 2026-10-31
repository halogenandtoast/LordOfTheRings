module LordOfTheRings.Enemy.Cards.PatrolSentry (patrolSentry) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (placementController)
import LordOfTheRings.Scenario.Scenarios.IntoThePit.Helpers (caveTorchExhausts)

newtype PatrolSentry = PatrolSentry EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

patrolSentry :: EnemyCard PatrolSentry
patrolSentry = enemy PatrolSentry Cards.patrolSentry

-- | Forced: After Cave Torch exhausts, Patrol Sentry makes an immediate attack against the
-- engaged player, if able.
instance HasAbilities PatrolSentry where
  getAbilities (PatrolSentry a) =
    [restricted a 1 (exists $ EnemyWithId a.id <> EnemyEngagedWith AnyPlayer) $ forced caveTorchExhausts]

-- | Shadow: Add Patrol Sentry to the staging area.
instance RunMessage PatrolSentry where
  runMessage msg e@(PatrolSentry attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ (placementController attrs.placement) \pid -> immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> PatrolSentry <$> runMessage msg attrs
