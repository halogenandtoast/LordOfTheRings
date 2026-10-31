module LordOfTheRings.Enemy.Cards.OrcRaidersNightmare (orcRaidersNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (rescuedVillagerOr)

newtype OrcRaidersNightmare = OrcRaidersNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

orcRaidersNightmare :: EnemyCard OrcRaidersNightmare
orcRaidersNightmare = enemy OrcRaidersNightmare Cards.orcRaidersNightmare

-- | When Revealed: Either discard 1 villager token from Rescued Villagers, or Orc Raiders
-- makes an immediate attack against the first player.
-- Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage OrcRaidersNightmare where
  runMessage msg e@(OrcRaidersNightmare attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      rescuedVillagerOr attrs lead "orcRaiders.attack" $ immediateAttack lead attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> OrcRaidersNightmare <$> runMessage msg attrs
