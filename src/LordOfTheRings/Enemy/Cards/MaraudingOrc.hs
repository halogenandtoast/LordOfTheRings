module LordOfTheRings.Enemy.Cards.MaraudingOrc (maraudingOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (afterAttackIfDestroyed, villagerDies, villagerShadow)

newtype MaraudingOrc = MaraudingOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

maraudingOrc :: EnemyCard MaraudingOrc
maraudingOrc = enemy MaraudingOrc Cards.maraudingOrc

-- | Forced: After Marauding Orc attacks and destroys a character, place 1 damage token on
-- Dead Villagers.
-- Shadow: If this attack destroys a character, discard 1 villager token from Rescued
-- Villagers.
instance RunMessage MaraudingOrc where
  runMessage msg e@(MaraudingOrc attrs) = case msg of
    DoStep 1 (DealDamage (EnemySource eid) target _) | eid == attrs.id, isCharacter target -> do
      afterAttackIfDestroyed eid [target] $ villagerDies attrs
      MaraudingOrc <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid defenders | cid == attrs.cardId -> do
      villagerShadow attrs eid pid defenders
      pure e
    _ -> MaraudingOrc <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
