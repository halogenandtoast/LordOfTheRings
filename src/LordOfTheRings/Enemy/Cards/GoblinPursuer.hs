module LordOfTheRings.Enemy.Cards.GoblinPursuer (goblinPursuer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinPursuer = GoblinPursuer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinPursuer :: EnemyCard GoblinPursuer
goblinPursuer = enemy GoblinPursuer Cards.goblinPursuer

-- | Damaged characters cannot be declared as attackers or defenders against Goblin Pursuer.
instance HasModifiersFor GoblinPursuer where
  getModifiersFor (GoblinPursuer a) = modified a a [NotFoughtByDamaged]

-- | Shadow: Attacking enemy gets +1 attack (+3 attack instead if this attack is undefended).
instance RunMessage GoblinPursuer where
  runMessage msg e@(GoblinPursuer attrs) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid (ifUndefended defenders 3 1)
      pure e
    _ -> GoblinPursuer <$> runMessage msg attrs
