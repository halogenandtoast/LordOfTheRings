module LordOfTheRings.Enemy.Cards.SeaScorpion (seaScorpion) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins.Helpers (underwaterActive)

newtype SeaScorpion = SeaScorpion EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

seaScorpion :: EnemyCard SeaScorpion
seaScorpion = enemy SeaScorpion Cards.seaScorpion

-- | Sea-scorpion gets +2 attack if the active location is Underwater.
instance HasModifiersFor SeaScorpion where
  getModifiersFor (SeaScorpion a) = whenM underwaterActive $ modifySelf a [AttackModifier 2]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the active location is
-- Underwater).
instance RunMessage SeaScorpion where
  runMessage msg e@(SeaScorpion attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      underwater <- underwaterActive
      enemyGetsAttack (CardIdSource cid) eid (if underwater then 2 else 1)
      pure e
    _ -> SeaScorpion <$> runMessage msg attrs
