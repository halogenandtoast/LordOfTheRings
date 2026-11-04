module LordOfTheRings.Enemy.Cards.EasterlingRaider (easterlingRaider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Easterling))

newtype EasterlingRaider = EasterlingRaider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

easterlingRaider :: EnemyCard EasterlingRaider
easterlingRaider = enemy EasterlingRaider Cards.easterlingRaider

-- | Archery 1.
-- Easterling Raider gets +1 Attack and +1 Defense for each Easterling attachment on it.
instance HasModifiersFor EasterlingRaider where
  getModifiersFor (EasterlingRaider a) = do
    n <- length <$> attachedWithTrait Easterling (toTarget a)
    when (n > 0) $ modifySelf a [AttackModifier n, DefenseModifier n]

-- | Shadow: Attacking enemy gets +1 Attack (+2 Attack instead if it has an Easterling
-- attachment).
instance RunMessage EasterlingRaider where
  runMessage msg e@(EasterlingRaider attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      easterling <- notNull <$> attachedWithTrait Easterling (EnemyTarget eid)
      enemyGetsAttack attrs eid (if easterling then 2 else 1)
      pure e
    _ -> EasterlingRaider <$> runMessage msg attrs
