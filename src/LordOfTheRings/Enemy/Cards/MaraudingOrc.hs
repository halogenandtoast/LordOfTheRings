module LordOfTheRings.Enemy.Cards.MaraudingOrc (maraudingOrc) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardRescuedVillagers, villagerDies)

newtype MaraudingOrc = MaraudingOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

maraudingOrc :: EnemyCard MaraudingOrc
maraudingOrc = enemy MaraudingOrc Cards.maraudingOrc

-- | Forced: After Marauding Orc attacks and destroys a character, place 1 damage token on
-- Dead Villagers.
instance HasAbilities MaraudingOrc where
  getAbilities (MaraudingOrc a) = [mkAbility a 1 $ forced $ CharacterDestroyedByAttackWindow After (Just a.id)]

-- | Shadow: If this attack destroys a character, discard 1 villager token from Rescued
-- Villagers.
instance RunMessage MaraudingOrc where
  runMessage msg e@(MaraudingOrc attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      villagerDies attrs
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      discardRescuedVillagers attrs 1
      pure e
    _ -> MaraudingOrc <$> runMessage msg attrs
