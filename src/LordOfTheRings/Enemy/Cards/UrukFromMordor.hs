module LordOfTheRings.Enemy.Cards.UrukFromMordor (urukFromMordor) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UrukFromMordor = UrukFromMordor EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urukFromMordor :: EnemyCard UrukFromMordor
urukFromMordor = enemy UrukFromMordor Cards.urukFromMordor

{- | When Revealed: Uruk from Mordor makes an immediate attack against the first player.
Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, attacking
enemy makes an additional attack.
-}
instance RunMessage UrukFromMordor where
  runMessage msg e@(UrukFromMordor attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      immediateAttack lead attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      immediateAttack pid eid
      pure e
    _ -> UrukFromMordor <$> runMessage msg attrs
