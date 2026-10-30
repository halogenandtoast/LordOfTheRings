module LordOfTheRings.Enemy.Cards.CorneredOrc (corneredOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CorneredOrc = CorneredOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

corneredOrc :: EnemyCard CorneredOrc
corneredOrc = enemy CorneredOrc Cards.corneredOrc

-- | Forced: After Cornered Orc engages a player, it makes an immediate attack.
instance HasAbilities CorneredOrc where
  getAbilities (CorneredOrc a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character is
-- damaged).
instance RunMessage CorneredOrc where
  runMessage msg e@(CorneredOrc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      damaged <- anyM (fmap (> 0) . getDamage) defenders
      enemyGetsAttack (CardIdSource cid) eid (if damaged then 2 else 1)
      pure e
    _ -> CorneredOrc <$> runMessage msg attrs
