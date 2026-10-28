module LordOfTheRings.Enemy.Cards.BatteringRam (batteringRam) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (besiegerShadow)
import LordOfTheRings.Trait (Trait (Battleground))

newtype BatteringRam = BatteringRam EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

batteringRam :: EnemyCard BatteringRam
batteringRam = enemy BatteringRam Cards.batteringRam

-- | When Revealed: If the active location is a Battleground, deal 3 damage to it.
-- Shadow: Deal 2 damage to The Approach if it is in play. Otherwise, attacking enemy gets
-- +2 attack.
instance RunMessage BatteringRam where
  runMessage msg e@(BatteringRam attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- select (IsActiveLocation <> LocationWithTrait Battleground)
      for_ active \l -> dealDamage attrs l 3
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      besiegerShadow attrs Locations.theApproach eid
      pure e
    _ -> BatteringRam <$> runMessage msg attrs
