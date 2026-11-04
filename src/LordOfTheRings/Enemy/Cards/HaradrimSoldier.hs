module LordOfTheRings.Enemy.Cards.HaradrimSoldier (haradrimSoldier) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype HaradrimSoldier = HaradrimSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

haradrimSoldier :: EnemyCard HaradrimSoldier
haradrimSoldier = enemy HaradrimSoldier Cards.haradrimSoldier

-- | Forced: After Haradrim Soldier engages you, it makes an immediate attack.
instance HasAbilities HaradrimSoldier where
  getAbilities (HaradrimSoldier a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage HaradrimSoldier where
  runMessage msg e@(HaradrimSoldier attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == attrs.id]) \pid -> immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> HaradrimSoldier <$> runMessage msg attrs
