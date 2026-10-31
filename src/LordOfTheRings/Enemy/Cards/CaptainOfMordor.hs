module LordOfTheRings.Enemy.Cards.CaptainOfMordor (captainOfMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CaptainOfMordor = CaptainOfMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

captainOfMordor :: EnemyCard CaptainOfMordor
captainOfMordor = enemy CaptainOfMordor Cards.captainOfMordor

-- | Each enemy gets -20 engagement cost.
instance HasModifiersFor CaptainOfMordor where
  getModifiersFor (CaptainOfMordor a) = do
    enemies <- select AnyEnemy
    for_ enemies \x -> modified a x [EngagementCostModifier (-20)]

-- | Shadow: If this attack destroys a character, attacking enemy cannot take damage this
-- round.
instance RunMessage CaptainOfMordor where
  runMessage msg e@(CaptainOfMordor attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      roundModifier attrs eid CannotTakeDamage
      pure e
    _ -> CaptainOfMordor <$> runMessage msg attrs
