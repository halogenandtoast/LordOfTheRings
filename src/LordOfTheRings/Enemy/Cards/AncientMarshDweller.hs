module LordOfTheRings.Enemy.Cards.AncientMarshDweller (ancientMarshDweller) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype AncientMarshDweller = AncientMarshDweller EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientMarshDweller :: EnemyCard AncientMarshDweller
ancientMarshDweller = enemy AncientMarshDweller Cards.ancientMarshDweller

-- | Cannot have attachments. Ancient Marsh-dweller gets +1 threat and +1 attack for each
-- resource token on it.
instance HasModifiersFor AncientMarshDweller where
  getModifiersFor (AncientMarshDweller a) = do
    let n = countTokens Resource (enemyTokens a)
    modified a a $ CannotHaveAttachments : [m | n > 0, m <- [ThreatModifier n, AttackModifier n]]

-- | Forced: After any number of time counters are removed from the current quest, place a
-- resource token here.
instance HasAbilities AncientMarshDweller where
  getAbilities (AncientMarshDweller a) = [mkAbility a 1 $ forced $ TimeCountersRemovedFromQuestWindow After]

instance RunMessage AncientMarshDweller where
  runMessage msg e@(AncientMarshDweller attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeResources attrs attrs 1
      pure e
    _ -> AncientMarshDweller <$> runMessage msg attrs
