module LordOfTheRings.Enemy.Cards.GoblinWarlord (goblinWarlord) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (triggerLostEffects)

newtype GoblinWarlord = GoblinWarlord EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinWarlord :: EnemyCard GoblinWarlord
goblinWarlord = enemy GoblinWarlord Cards.goblinWarlord

-- | Lost: Each player must choose and discard 1 ally he controls from play, if able.
-- Shadow: Trigger all "Lost:" effects in play.
instance RunMessage GoblinWarlord where
  runMessage msg e@(GoblinWarlord attrs) = case msg of
    TriggerLostEffects _ -> do
      eachPlayer \p -> chooseAndDiscard attrs p . map toTarget =<< select (AllyControlledBy $ PlayerWithId p)
      pure e
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      triggerLostEffects attrs
      pure e
    _ -> GoblinWarlord <$> runMessage msg attrs
