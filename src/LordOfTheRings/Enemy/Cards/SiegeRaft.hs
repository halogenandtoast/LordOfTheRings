module LordOfTheRings.Enemy.Cards.SiegeRaft (siegeRaft) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (besiegerShadow, lowestThreatBattleground)

newtype SiegeRaft = SiegeRaft EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

siegeRaft :: EnemyCard SiegeRaft
siegeRaft = enemy SiegeRaft Cards.siegeRaft

-- | When Revealed: Deal 2 damage to the lowest threat Battleground location in play, if
-- able.
-- Shadow: Deal 2 damage to The Banks if it is in play. Otherwise, attacking enemy gets +2
-- attack.
instance RunMessage SiegeRaft where
  runMessage msg e@(SiegeRaft attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lowest <- lowestThreatBattleground
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "siegeRaft.choose"
        causedBy attrs
        targets lowest \l -> dealDamage attrs l 2
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      besiegerShadow attrs Locations.theBanks eid
      pure e
    _ -> SiegeRaft <$> runMessage msg attrs
