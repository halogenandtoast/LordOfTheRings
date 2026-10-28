module LordOfTheRings.Enemy.Cards.DruadanElite (druadanElite) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (returnToStagingUnlessAnyPlayerPays)
import LordOfTheRings.Sphere (Sphere (Neutral))

newtype DruadanElite = DruadanElite EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

druadanElite :: EnemyCard DruadanElite
druadanElite = enemy DruadanElite Cards.druadanElite

-- | Archery 2. Prowl X. X is the number of players in the game.
-- Shadow: Return attacking enemy to the staging area after it attacks unless any player
-- pays 1 resource.
instance RunMessage DruadanElite where
  runMessage msg e@(DruadanElite attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      spendGroupResources lead Neutral . length =<< getRemainingPlayers
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingUnlessAnyPlayerPays attrs eid
      pure e
    _ -> DruadanElite <$> runMessage msg attrs
