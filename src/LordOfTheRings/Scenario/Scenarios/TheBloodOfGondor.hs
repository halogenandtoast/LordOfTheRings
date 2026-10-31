module LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor (theBloodOfGondor) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheBloodOfGondor = TheBloodOfGondor ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBloodOfGondor :: Scenario
theBloodOfGondor = Scenario $ scenario TheBloodOfGondor "the-blood-of-gondor" "The Blood of Gondor"

instance RunMessage TheBloodOfGondor where
  runMessage msg (TheBloodOfGondor attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheBloodOfGondor, EncounterSet.RavagingOrcs]
      firstStage <- genEncounterCard Quests.theAmbush
      laterStages <- traverse genEncounterCard [Quests.captured]
      putQuestIntoPlay firstStage
      pure $ TheBloodOfGondor $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheBloodOfGondor <$> runMessage msg attrs
