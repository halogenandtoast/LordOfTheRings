module LordOfTheRings.Enemy.Cards.GoblinTroopDarkOfMirkwood (goblinTroopDarkOfMirkwood) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Trait (Trait (Goblin))

newtype GoblinTroopDarkOfMirkwood = GoblinTroopDarkOfMirkwood EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTroopDarkOfMirkwood :: EnemyCard GoblinTroopDarkOfMirkwood
goblinTroopDarkOfMirkwood = enemy GoblinTroopDarkOfMirkwood Cards.goblinTroopDarkOfMirkwood

-- | Cannot have attachments. While Goblin Troop is engaged with you, each other
-- Goblin enemy engaged with you gets +1 attack and +1 defense.
instance HasModifiersFor GoblinTroopDarkOfMirkwood where
  getModifiersFor (GoblinTroopDarkOfMirkwood a) = do
    modifySelf a [CannotHaveAttachments]
    case a.placement of
      EngagedWith pid -> do
        goblins <- select (EnemyEngagedWith (PlayerWithId pid) <> EnemyWithTrait Goblin <> not_ (EnemyWithId a.id))
        for_ goblins \e -> modified a e [AttackModifier 1, DefenseModifier 1]
      _ -> pure ()

-- | Shadow: Attacking enemy gets +2 attack.
instance RunMessage GoblinTroopDarkOfMirkwood where
  runMessage msg e@(GoblinTroopDarkOfMirkwood attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> GoblinTroopDarkOfMirkwood <$> runMessage msg attrs
