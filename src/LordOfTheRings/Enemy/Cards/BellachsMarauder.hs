module LordOfTheRings.Enemy.Cards.BellachsMarauder (bellachsMarauder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype BellachsMarauder = BellachsMarauder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bellachsMarauder :: EnemyCard BellachsMarauder
bellachsMarauder = enemy BellachsMarauder Cards.bellachsMarauder

-- | While Bellach's Marauder is engaged with a player, it gains: "Forced: After a time
-- counter is removed from the current quest, deal Bellach's Marauder 2 shadow cards."
instance HasAbilities BellachsMarauder where
  getAbilities (BellachsMarauder a) = case a.placement of
    EngagedWith _ -> [mkAbility a 1 $ forced $ TimeCountersRemovedFromQuestWindow After]
    _ -> []

-- | Shadow: Remove 1 time counter from the current quest.
instance RunMessage BellachsMarauder where
  runMessage msg e@(BellachsMarauder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      replicateM_ 2 $ dealShadowCard attrs.id
      pure e
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure e
    _ -> BellachsMarauder <$> runMessage msg attrs
