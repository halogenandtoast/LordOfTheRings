module LordOfTheRings.Enemy.Cards.HowlingWarg (howlingWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (damageAnActiveLocation)

newtype HowlingWarg = HowlingWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

howlingWarg :: EnemyCard HowlingWarg
howlingWarg = enemy HowlingWarg Cards.howlingWarg

-- | Forced: When Howling Warg attacks, place 1 damage on an active location.
instance HasAbilities HowlingWarg where
  getAbilities (HowlingWarg a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, place 1
-- damage on an active location.
instance RunMessage HowlingWarg where
  runMessage msg e@(HowlingWarg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      damageAnActiveLocation attrs 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      damageAnActiveLocation attrs 1
      pure e
    _ -> HowlingWarg <$> runMessage msg attrs
