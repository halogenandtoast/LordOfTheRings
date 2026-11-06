module LordOfTheRings.Enemy.Cards.BlueMountainGoblin (blueMountainGoblin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg.Helpers (lootControlled)
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (playerControlsLoot)

newtype BlueMountainGoblin = BlueMountainGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

blueMountainGoblin :: EnemyCard BlueMountainGoblin
blueMountainGoblin = enemy BlueMountainGoblin Cards.blueMountainGoblin

instance HasModifiersFor BlueMountainGoblin where
  getModifiersFor (BlueMountainGoblin a) = do
    loot <- lootControlled
    when (loot > 0) $ modifySelf a [GainKeyword Surge]

instance RunMessage BlueMountainGoblin where
  runMessage msg e@(BlueMountainGoblin a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      whenM (playerControlsLoot pid) do
        putShadowEnemyIntoPlay eid cid pid
        doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      select (EnemyWithCardId cid) >>= traverse_ dealShadowCard
      pure e
    _ -> BlueMountainGoblin <$> runMessage msg a
