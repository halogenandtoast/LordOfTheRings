module LordOfTheRings.Enemy.Cards.SouthronWarrior (southronWarrior) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SouthronWarrior = SouthronWarrior EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronWarrior :: EnemyCard SouthronWarrior
southronWarrior = enemy SouthronWarrior Cards.southronWarrior

instance HasAbilities SouthronWarrior where
  getAbilities (SouthronWarrior a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage SouthronWarrior where
  runMessage msg e@(SouthronWarrior a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == a.id]) \pid -> do
        allies <- map toTarget <$> select (AllyControlledBy $ PlayerWithId pid)
        chooseAndDiscard a pid allies
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      damaged <- anyM (fmap (> 0) . getDamage) defenders
      enemyGetsAttack a eid (if damaged then 2 else 1)
      pure e
    _ -> SouthronWarrior <$> runMessage msg a
