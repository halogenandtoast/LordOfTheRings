module LordOfTheRings.Enemy.Cards.RavenousSpider (ravenousSpider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RavenousSpider = RavenousSpider EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenousSpider :: EnemyCard RavenousSpider
ravenousSpider = enemy RavenousSpider Cards.ravenousSpider

-- | Forced: After Ravenous Spider engages you, it makes an immediate attack.
instance HasAbilities RavenousSpider where
  getAbilities (RavenousSpider a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy makes an additional attack against you after this one.
instance RunMessage RavenousSpider where
  runMessage msg e@(RavenousSpider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged eid pid) <- ws, eid == attrs.id]) \pid -> immediateAttack pid attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> RavenousSpider <$> runMessage msg attrs
