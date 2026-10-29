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
-- That ends the current attack, dealing no damage; its shadow cards not yet resolved are
-- discarded as the new attack is dealt its own (ruling). With no next player it is not
-- able, and the attack resolves as normal.
instance RunMessage MorgulBodyguard where
  runMessage msg e@(MorgulBodyguard attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      getNextPlayer pid >>= traverse_ \next -> do
        cancelMatching \case
          AttackDamage eid' _ -> eid' == eid
          UndefendedDamage _ eid' -> eid' == eid
          DefendedByMany _ eid' _ -> eid' == eid
          AttackDefended _ eid' _ -> eid' == eid
          RevealShadowCard eid' _ -> eid' == eid
          ResolveShadowCard _ eid' _ _ -> eid' == eid
          CheckWindows ws -> any (\case Window _ (ShadowEffectTriggered _ eid') -> eid' == eid; _ -> False) ws
          _ -> False
        push $ DiscardShadowCards eid
        engageEnemy next eid
        immediateAttack next eid
      pure e
    _ -> MorgulBodyguard <$> runMessage msg attrs
