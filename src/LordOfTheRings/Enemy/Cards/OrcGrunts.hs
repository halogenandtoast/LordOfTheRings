module LordOfTheRings.Enemy.Cards.OrcGrunts (orcGrunts) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype OrcGrunts = OrcGrunts EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 1. Surge.
orcGrunts :: EnemyCard OrcGrunts
orcGrunts = enemy OrcGrunts Cards.orcGrunts

-- | Any time progress is placed on the current quest, reduce the amount of progress placed
-- by 1.
instance HasModifiersFor OrcGrunts where
  getModifiersFor (OrcGrunts a) = getCurrentQuest >>= traverse_ \q -> modified a q [ProgressReducedBy 1]

-- | Shadow: Add Orc Grunts to the staging area.
instance RunMessage OrcGrunts where
  runMessage msg e@(OrcGrunts attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> OrcGrunts <$> runMessage msg attrs
