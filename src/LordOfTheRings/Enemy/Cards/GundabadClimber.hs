module LordOfTheRings.Enemy.Cards.GundabadClimber (gundabadClimber) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GundabadClimber = GundabadClimber EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gundabadClimber :: EnemyCard GundabadClimber
gundabadClimber = enemy GundabadClimber Cards.gundabadClimber

-- | When Revealed: Remove 1 progress from every quest stage in play. (2 progress instead if
-- 3 or more players in the game.)
instance RunMessage GundabadClimber where
  runMessage msg e@(GundabadClimber attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      removeProgressFromMainQuest attrs (if players >= 3 then 2 else 1)
      pure e
    _ -> GundabadClimber <$> runMessage msg attrs
