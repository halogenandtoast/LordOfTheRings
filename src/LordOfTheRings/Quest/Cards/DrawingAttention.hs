module LordOfTheRings.Quest.Cards.DrawingAttention (drawingAttention) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast.Helpers

newtype DrawingAttention = DrawingAttention QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

drawingAttention :: QuestCard DrawingAttention
drawingAttention = quest DrawingAttention Cards.drawingAttention

-- | City Guard cannot have more than 15 damage, and it gets -5 engagement cost for each
-- resource here.
instance HasModifiersFor DrawingAttention where
  getModifiersFor (DrawingAttention a) = cityGuardStageModifiers a 15

-- | Forced: At the end of the round, place 1 resource here.
instance HasAbilities DrawingAttention where
  getAbilities (DrawingAttention a) = cityGuardStageAbilities a

-- | When Revealed: Each player must choose: either raise your threat by 5, or City Guard
-- makes an immediate attack against you.
instance RunMessage DrawingAttention where
  runMessage msg q@(DrawingAttention attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      DrawingAttention <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      guard' <- getCityGuard
      eachPlayer \p -> chooseOrRunOneM p do
        questionLabeled "drawingAttention.choose"
        causedBy attrs
        labeled "drawingAttention.threat" $ encounterRaisesThreat p 5
        for_ guard' \g -> labeled "drawingAttention.attack" $ immediateAttack p g
      pure q
    _ -> DrawingAttention <$> runCityGuardStage msg attrs
