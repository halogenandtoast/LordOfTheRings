module LordOfTheRings.Enemy.Cards.MorgulBodyguard (morgulBodyguard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Captain))

newtype MorgulBodyguard = MorgulBodyguard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulBodyguard :: EnemyCard MorgulBodyguard
morgulBodyguard = enemy MorgulBodyguard Cards.morgulBodyguard

-- | Any damage that would be dealt to a Captain enemy is dealt to 1 Morgul Bodyguard
-- instead: with several in play, the first player picks which.
instance HasModifiersFor MorgulBodyguard where
  getModifiersFor (MorgulBodyguard a) = do
    captains <- select (EnemyWithTrait Captain)
    for_ captains \c -> modified a c [DamageRedirectedTo (toTarget a)]

-- | Shadow: Attacking enemy engages the next player and makes an immediate attack, if able.
instance RunMessage MorgulBodyguard where
  runMessage msg e@(MorgulBodyguard attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      getNextPlayer pid >>= traverse_ \next -> do
        engageEnemy next eid
        immediateAttack next eid
      pure e
    _ -> MorgulBodyguard <$> runMessage msg attrs
