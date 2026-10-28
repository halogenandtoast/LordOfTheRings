module LordOfTheRings.Enemy.Cards.BrutalUruk (brutalUruk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (attackDestroys, turnHiddenCardsFaceup)

newtype BrutalUruk = BrutalUruk EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brutalUruk :: EnemyCard BrutalUruk
brutalUruk = enemy BrutalUruk Cards.brutalUruk

-- | Forced: After Brutal Uruk attacks and destroys a character, that character's controller
-- turns all hidden cards in his play area faceup. Deal a shadow card to each enemy turned
-- faceup by this effect and resolve their attacks as normal.
-- Its attack's damage tells, as it is dealt, whose character it destroys; the effect then
-- waits for the attack to end.
instance RunMessage BrutalUruk where
  runMessage msg e@(BrutalUruk attrs) = case msg of
    DoStep 1 (DealDamage (EnemySource eid) target n) | eid == attrs.id -> do
      attackDestroys target n >>= traverse_ \pid ->
        afterAttack attrs.id [UseCardAbility pid (toSource attrs) 1 []]
      BrutalUruk <$> runMessage msg attrs
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      enemies <- turnHiddenCardsFaceup pid
      for_ enemies \enemy' -> whileInPlay enemy' $ dealShadowCard enemy'
      for_ enemies \enemy' -> whileInPlay enemy' $ push $ EnemyAttack pid enemy'
      pure e
    _ -> BrutalUruk <$> runMessage msg attrs
