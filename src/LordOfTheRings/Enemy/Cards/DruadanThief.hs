module LordOfTheRings.Enemy.Cards.DruadanThief (druadanThief) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDruadanForest.Helpers (returnToStagingUnlessAnyPlayerPays)

newtype DruadanThief = DruadanThief EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

druadanThief :: EnemyCard DruadanThief
druadanThief = enemy DruadanThief Cards.druadanThief

-- | Surge. Prowl 1.
-- Shadow: Return attacking enemy to the staging area after it attacks unless any player
-- pays 1 resource.
instance RunMessage DruadanThief where
  runMessage msg e@(DruadanThief attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingUnlessAnyPlayerPays attrs eid
      pure e
    _ -> DruadanThief <$> runMessage msg attrs
