module LordOfTheRings.Enemy.Cards.PitGoblin (pitGoblin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (addResourceIfGoblin)

newtype PitGoblin = PitGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pitGoblin :: EnemyCard PitGoblin
pitGoblin = enemy PitGoblin Cards.pitGoblin

-- | When Revealed: Add 2 resources to Pit Goblin.
-- Shadow: If the attacking enemy is a Goblin, add 1 resource to it.
instance RunMessage PitGoblin where
  runMessage msg e@(PitGoblin attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeResources attrs attrs 2
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      addResourceIfGoblin attrs eid
      pure e
    _ -> PitGoblin <$> runMessage msg attrs
