module LordOfTheRings.Enemy.Cards.MorgulTracker (morgulTracker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower)

newtype MorgulTracker = MorgulTracker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulTracker :: EnemyCard MorgulTracker
morgulTracker = enemy MorgulTracker Cards.morgulTracker

-- | Forced: When Morgul Tracker is optionally engaged, place 1 progress token on To the
-- Tower.
instance HasAbilities MorgulTracker where
  getAbilities (MorgulTracker a) = [mkAbility a 1 $ forced $ EnemyOptionallyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack. If undefended, place 1 progress token on To the
-- Tower as well.
instance RunMessage MorgulTracker where
  runMessage msg e@(MorgulTracker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeOnTower attrs 1
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      when (null defenders) $ placeOnTower attrs 1
      pure e
    _ -> MorgulTracker <$> runMessage msg attrs
