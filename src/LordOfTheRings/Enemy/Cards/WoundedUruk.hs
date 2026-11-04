module LordOfTheRings.Enemy.Cards.WoundedUruk (woundedUruk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WoundedUruk = WoundedUruk EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Wounded Uruk enters play with 2 damage on it.
woundedUruk :: EnemyCard WoundedUruk
woundedUruk = enemy (WoundedUruk . \a -> a {enemyTokens = addTokens Damage 2 a.enemyTokens}) Cards.woundedUruk

-- | Forced: After Wounded Uruk engages you, it gets +2 attack until the end of the round.
instance HasAbilities WoundedUruk where
  getAbilities (WoundedUruk a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack for each damage on it.
instance RunMessage WoundedUruk where
  runMessage msg e@(WoundedUruk attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifier attrs attrs.id (AttackModifier 2)
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      damage <- (.damage) <$> getEnemy eid
      when (damage > 0) $ enemyGetsAttack (CardIdSource cid) eid damage
      pure e
    _ -> WoundedUruk <$> runMessage msg attrs
