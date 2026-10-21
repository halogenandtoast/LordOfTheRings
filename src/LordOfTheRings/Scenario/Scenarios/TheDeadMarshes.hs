module LordOfTheRings.Scenario.Scenarios.TheDeadMarshes (theDeadMarshes) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheDeadMarshes = TheDeadMarshes ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDeadMarshes :: Scenario
theDeadMarshes = Scenario $ scenario TheDeadMarshes "the-dead-marshes" "The Dead Marshes"

instance RunMessage TheDeadMarshes where
  runMessage msg (TheDeadMarshes attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- concatMapM (gatherEncounterSet attrs) [EncounterSet.TheDeadMarshes, EncounterSet.SauronsReach]
      firstStage <- genEncounterCard Quests.intoTheMarshes
      laterStages <- traverse genEncounterCard [Quests.theCapture]
      putQuestIntoPlay firstStage
      pure $ TheDeadMarshes $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheDeadMarshes <$> runMessage msg attrs
