module LordOfTheRings.Enemy.Cards.CarnDumGarrison (carnDumGarrison) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CarnDumGarrison = CarnDumGarrison EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

carnDumGarrison :: EnemyCard CarnDumGarrison
carnDumGarrison = enemy CarnDumGarrison Cards.carnDumGarrison

-- | Carn Dûm Garrison gets +1 attack for each shadow card currently dealt to it.
instance HasModifiersFor CarnDumGarrison where
  getModifiersFor (CarnDumGarrison a) = modifySelf a [AttackModifier (length a.shadowCards) | notNull a.shadowCards]

-- | Shadow: Attacking enemy gets +1 attack for each shadow card currently dealt to it, this
-- one among them.
instance RunMessage CarnDumGarrison where
  runMessage msg e@(CarnDumGarrison attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      n <- length . (.shadowCards) <$> getEnemy eid
      enemyGetsAttack attrs eid n
      pure e
    _ -> CarnDumGarrison <$> runMessage msg attrs
