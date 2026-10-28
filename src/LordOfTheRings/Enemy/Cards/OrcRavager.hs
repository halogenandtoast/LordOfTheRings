module LordOfTheRings.Enemy.Cards.OrcRavager (orcRavager) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardVillagers)

newtype OrcRavager = OrcRavager EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcRavager :: EnemyCard OrcRavager
orcRavager = enemy OrcRavager Cards.orcRavager

-- | When Revealed: Discard 1 villager token from the active location. If no villager token
-- is discarded by this effect, Orc Ravager gains Surge.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if undefended.)
instance RunMessage OrcRavager where
  runMessage msg e@(OrcRavager attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      discarded <- sum <$> (traverse (\l -> discardVillagers attrs l 1) =<< select IsActiveLocation)
      when (discarded == 0) revealEncounterCard
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid (ifUndefended defenders 2 1)
      pure e
    _ -> OrcRavager <$> runMessage msg attrs
