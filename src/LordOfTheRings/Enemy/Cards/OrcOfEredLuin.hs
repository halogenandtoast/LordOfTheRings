module LordOfTheRings.Enemy.Cards.OrcOfEredLuin (orcOfEredLuin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (playerControlsLoot)

newtype OrcOfEredLuin = OrcOfEredLuin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcOfEredLuin :: EnemyCard OrcOfEredLuin
orcOfEredLuin = enemy OrcOfEredLuin Cards.orcOfEredLuin

instance RunMessage OrcOfEredLuin where
  runMessage msg e@(OrcOfEredLuin a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      order <- getPlayerOrder
      for_ order \pid -> whenM (playerControlsLoot pid) $ immediateAttack pid a.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      -- FAQ 1.46: a solo game has no "next" player, so neither part happens.
      attackNextPlayerAfterAttack eid pid
      pure e
    _ -> OrcOfEredLuin <$> runMessage msg a
