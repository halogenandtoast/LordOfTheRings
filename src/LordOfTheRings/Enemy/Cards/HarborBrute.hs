module LordOfTheRings.Enemy.Cards.HarborBrute (harborBrute) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (enemyProgress, getScroll, scrollEnemy)

newtype HarborBrute = HarborBrute EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

harborBrute :: EnemyCard HarborBrute
harborBrute = enemy HarborBrute Cards.harborBrute

-- | When Revealed: If Alcaron's Scroll is attached to an enemy, detach it from that enemy
-- and attach it to Harbor Brute. Move all progress tokens from that enemy to Harbor Brute.
instance RunMessage HarborBrute where
  runMessage msg e@(HarborBrute attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      holder <- scrollEnemy
      scroll <- getScroll
      for_ ((,) <$> holder <*> scroll) \(from, oid) -> when (from /= attrs.id) do
        attachObjective oid attrs.id
        progress <- enemyProgress from
        when (progress > 0) do
          removeTokens attrs from Progress progress
          placeProgress attrs attrs progress
      pure e
    _ -> HarborBrute <$> runMessage msg attrs
