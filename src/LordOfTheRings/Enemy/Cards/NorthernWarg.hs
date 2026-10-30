module LordOfTheRings.Enemy.Cards.NorthernWarg (northernWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Warg))

newtype NorthernWarg = NorthernWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

northernWarg :: EnemyCard NorthernWarg
northernWarg = enemy NorthernWarg Cards.northernWarg

-- | When Revealed: Each Warg enemy gets +1 threat until the end of the phase.
-- Shadow: After this attack, attacking enemy engages the next player, then makes an
-- immediate attack (deal a new shadow card for that attack). With no next player, nothing.
instance RunMessage NorthernWarg where
  runMessage msg e@(NorthernWarg attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      wargs <- select (EnemyWithTrait Warg)
      for_ wargs \w -> phaseModifier attrs w (ThreatModifier 1)
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      getNextPlayer pid >>= traverse_ \next -> do
        again <- capture do
          engageEnemy next eid
          immediateAttack next eid
        afterAttack eid again
      pure e
    _ -> NorthernWarg <$> runMessage msg attrs
