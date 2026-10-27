module LordOfTheRings.Enemy.Cards.GoblinSwordsman (goblinSwordsman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinSwordsman = GoblinSwordsman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinSwordsman :: EnemyCard GoblinSwordsman
goblinSwordsman = enemy GoblinSwordsman Cards.goblinSwordsman

-- | Goblin Swordsman gets +2 [Attack] if its attack is undefended.
instance HasModifiersFor GoblinSwordsman where
  getModifiersFor (GoblinSwordsman a) = modified a a [AttackModifierIfUndefended 2]

-- | Shadow: Add Goblin Swordsman to the staging area.
instance RunMessage GoblinSwordsman where
  runMessage msg e@(GoblinSwordsman attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> GoblinSwordsman <$> runMessage msg attrs
