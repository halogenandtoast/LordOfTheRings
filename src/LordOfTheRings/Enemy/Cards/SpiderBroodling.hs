module LordOfTheRings.Enemy.Cards.SpiderBroodling (spiderBroodling) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (isOnTrack)

newtype SpiderBroodling = SpiderBroodling EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

spiderBroodling :: EnemyCard SpiderBroodling
spiderBroodling = enemy SpiderBroodling Cards.spiderBroodling

-- | While On Track is in play, Spider Broodling gains surge.
instance HasModifiersFor SpiderBroodling where
  getModifiersFor (SpiderBroodling a) = whenM isOnTrack $ modifySelf a [GainKeyword Surge]

-- | Shadow: If On Track is in play, put Spider Broodling into play engaged with you and deal
-- it a shadow card.
instance RunMessage SpiderBroodling where
  runMessage msg e@(SpiderBroodling attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM isOnTrack do
        putShadowEnemyIntoPlay eid cid pid
        doStep 1 msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      dealShadowCard attrs.id
      pure e
    _ -> SpiderBroodling <$> runMessage msg attrs
