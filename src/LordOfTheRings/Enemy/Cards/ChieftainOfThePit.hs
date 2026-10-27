module LordOfTheRings.Enemy.Cards.ChieftainOfThePit (chieftainOfThePit) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype ChieftainOfThePit = ChieftainOfThePit EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chieftainOfThePit :: EnemyCard ChieftainOfThePit
chieftainOfThePit = enemy ChieftainOfThePit Cards.chieftainOfThePit

-- | When Revealed: Chieftain of the Pit gets +3 attack until the end of the round.
-- Shadow: attacking enemy attacks again after this attack. Deal it another shadow card for
-- the next attack.
instance RunMessage ChieftainOfThePit where
  runMessage msg e@(ChieftainOfThePit attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      roundModifier attrs attrs (AttackModifier 3)
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      afterAttack eid [DealShadowCard eid, EnemyAttack pid eid]
      pure e
    _ -> ChieftainOfThePit <$> runMessage msg attrs
