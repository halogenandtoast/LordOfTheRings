module LordOfTheRings.Enemy.Cards.UlchorsAgent (ulchorsAgent) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UlchorsAgent = UlchorsAgent EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ulchorsAgent :: EnemyCard UlchorsAgent
ulchorsAgent = enemy UlchorsAgent Cards.ulchorsAgent

-- | When Revealed: Raise each player's threat by 3.
-- Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, raise each
-- player's threat by 1.
instance RunMessage UlchorsAgent where
  runMessage msg e@(UlchorsAgent attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> encounterRaisesThreat p 3
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure e
    _ -> UlchorsAgent <$> runMessage msg attrs
