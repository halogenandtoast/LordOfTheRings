module LordOfTheRings.Enemy.Cards.IsengardTracker (isengardTracker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BreakingOfTheFellowship.Helpers (toFirstPlayersStagingArea)

newtype IsengardTracker = IsengardTracker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

isengardTracker :: EnemyCard IsengardTracker
isengardTracker = enemy IsengardTracker Cards.isengardTracker

instance HasAbilities IsengardTracker where
  getAbilities (IsengardTracker a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

instance RunMessage IsengardTracker where
  runMessage msg e@(IsengardTracker a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      toFirstPlayersStagingArea a
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      afterAttack eid [DoStep 1 msg]
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      toFirstPlayersStagingArea (EnemyTarget eid)
      pure e
    _ -> IsengardTracker <$> runMessage msg a
