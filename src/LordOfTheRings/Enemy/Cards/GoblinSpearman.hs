module LordOfTheRings.Enemy.Cards.GoblinSpearman (goblinSpearman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinSpearman = GoblinSpearman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinSpearman :: EnemyCard GoblinSpearman
goblinSpearman = enemy GoblinSpearman Cards.goblinSpearman

-- | Goblin Spearman gets +2 attack if its attack is undefended.
instance HasModifiersFor GoblinSpearman where
  getModifiersFor (GoblinSpearman a) = modifySelf a [AttackModifierIfUndefended 2]

-- | Shadow: Add Goblin Spearman to the staging area.
instance RunMessage GoblinSpearman where
  runMessage msg e@(GoblinSpearman attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> GoblinSpearman <$> runMessage msg attrs
