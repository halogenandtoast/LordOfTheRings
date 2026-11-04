module LordOfTheRings.Enemy.Cards.SnagaArcher (snagaArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SnagaArcher = SnagaArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Archery 2.
snagaArcher :: EnemyCard SnagaArcher
snagaArcher = enemy SnagaArcher Cards.snagaArcher

-- | Forced: After Snaga Archer engages you, deal 2 damage to a character you control.
instance HasAbilities SnagaArcher where
  getAbilities (SnagaArcher a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character is
-- damaged).
instance RunMessage SnagaArcher where
  runMessage msg e@(SnagaArcher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageToOneCharacter attrs pid 2
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      damaged <- anyM (fmap (> 0) . getDamage) defenders
      enemyGetsAttack (CardIdSource cid) eid (if damaged then 2 else 1)
      pure e
    _ -> SnagaArcher <$> runMessage msg attrs
