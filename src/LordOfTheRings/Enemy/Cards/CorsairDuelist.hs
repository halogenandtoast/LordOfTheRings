module LordOfTheRings.Enemy.Cards.CorsairDuelist (corsairDuelist) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Keyword (Keyword (Indestructible))

newtype CorsairDuelist = CorsairDuelist EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Corsair Duelist enters play with 2 resources on it.
corsairDuelist :: EnemyCard CorsairDuelist
corsairDuelist = enemy (CorsairDuelist . \a -> a {enemyTokens = addTokens Resource 2 a.enemyTokens}) Cards.corsairDuelist

-- | Forced: When Corsair Duelist would be defeated, remove 1 resource and all damage from it
-- instead. While it has a resource, damage does not destroy it; once the damage reaches its
-- hit points, the resource and the damage go.
instance HasModifiersFor CorsairDuelist where
  getModifiersFor (CorsairDuelist a) = when (a.resources > 0) $ modifySelf a [GainKeyword Indestructible]

instance HasAbilities CorsairDuelist where
  getAbilities (CorsairDuelist a) = [mkAbility a 1 $ silent $ DealtDamageWindow After (toTarget a) | a.resources > 0]

-- | Shadow: Attacking enemy gets +1 attack for each resource on it.
instance RunMessage CorsairDuelist where
  runMessage msg e@(CorsairDuelist attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      hp <- getEnemyHitPoints attrs.id
      when (attrs.damage >= hp) do
        removeTokens attrs attrs.id Resource 1
        healAllDamage attrs attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- (.resources) <$> getEnemy eid
      when (n > 0) $ enemyGetsAttack (CardIdSource cid) eid n
      pure e
    _ -> CorsairDuelist <$> runMessage msg attrs
