module LordOfTheRings.Enemy.Cards.GoblinTroop (goblinTroop) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Goblin))

newtype GoblinTroop = GoblinTroop EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTroop :: EnemyCard GoblinTroop
goblinTroop = enemy GoblinTroop Cards.goblinTroop

-- | Cannot have player card attachments. While Goblin Troop is engaged with you, each other
-- Goblin enemy engaged with you gets +1 attack and +1 defense.
instance HasModifiersFor GoblinTroop where
  getModifiersFor (GoblinTroop a) = do
    modifySelf a [CannotHavePlayerAttachments]
    case a.placement of
      EngagedWith pid -> do
        goblins <- select (EnemyEngagedWith (PlayerWithId pid) <> EnemyWithTrait Goblin <> not_ (EnemyWithId a.id))
        for_ goblins \e -> modified a e [AttackModifier 1, DefenseModifier 1]
      _ -> pure ()

-- | Shadow: Attacking enemy gets +2 attack.
instance RunMessage GoblinTroop where
  runMessage msg e@(GoblinTroop attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> GoblinTroop <$> runMessage msg attrs
