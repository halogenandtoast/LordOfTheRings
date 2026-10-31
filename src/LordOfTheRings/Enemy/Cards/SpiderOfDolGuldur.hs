module LordOfTheRings.Enemy.Cards.SpiderOfDolGuldur (spiderOfDolGuldur) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))

newtype SpiderOfDolGuldur = SpiderOfDolGuldur EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

spiderOfDolGuldur :: EnemyCard SpiderOfDolGuldur
spiderOfDolGuldur = enemy SpiderOfDolGuldur Cards.spiderOfDolGuldur

-- | If there are any unclaimed objectives in play, Spider of Dol Guldur gains surge.
instance HasModifiersFor SpiderOfDolGuldur where
  getModifiersFor (SpiderOfDolGuldur a) = do
    n <- unclaimedObjectives
    when (n > 0) $ modifySelf a [GainKeyword Surge]

-- | Shadow: Attacking enemy gets +3 attack for each unclaimed objective in play.
instance RunMessage SpiderOfDolGuldur where
  runMessage msg e@(SpiderOfDolGuldur attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- unclaimedObjectives
      when (n > 0) $ enemyGetsAttack attrs eid (3 * n)
      pure e
    _ -> SpiderOfDolGuldur <$> runMessage msg attrs
