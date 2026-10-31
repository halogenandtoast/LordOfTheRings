module LordOfTheRings.Enemy.Cards.PillagingOrc (pillagingOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (getVillagers, rescuedVillagerOr)

newtype PillagingOrc = PillagingOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

pillagingOrc :: EnemyCard PillagingOrc
pillagingOrc = enemy PillagingOrc Cards.pillagingOrc

-- | When Revealed: Either discard 1 villager token from Rescued Villagers, or reveal an
-- additional encounter card.
-- Shadow: Deal attacking enemy 1 shadow card for each villager token on the active
-- location.
instance RunMessage PillagingOrc where
  runMessage msg e@(PillagingOrc attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      rescuedVillagerOr attrs lead "pillagingOrc.reveal" revealEncounterCard
      pure e
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      n <- sum <$> (traverse (getVillagers . toTarget) =<< select IsActiveLocation)
      when (n > 0) $ dealShadowCardsDuringAttack eid pid defenders n
      pure e
    _ -> PillagingOrc <$> runMessage msg attrs
