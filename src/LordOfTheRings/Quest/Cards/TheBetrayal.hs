module LordOfTheRings.Quest.Cards.TheBetrayal (theBetrayal) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (captainCannotLeavePlay, isCaptain)

newtype TheBetrayal = TheBetrayal QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBetrayal :: QuestCard TheBetrayal
theBetrayal = quest TheBetrayal Cards.theBetrayal

-- | Lord Alcaron cannot leave play unless destroyed.
instance HasModifiersFor TheBetrayal where
  getModifiersFor (TheBetrayal a) = captainCannotLeavePlay a Enemies.lordAlcaron

-- | When Revealed: Add Lord Alcaron to the staging area.
-- If Lord Alcaron is defeated, advance to the next stage.
instance RunMessage TheBetrayal where
  runMessage msg q@(TheBetrayal attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheBetrayal <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.lordAlcaron)
      pure q
    Destroy _ (EnemyTarget eid) -> do
      whenM (isCaptain Enemies.lordAlcaron eid) $ completeQuest attrs.id
      TheBetrayal <$> runMessage msg attrs
    _ -> TheBetrayal <$> runMessage msg attrs
