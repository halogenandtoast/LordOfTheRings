module LordOfTheRings.Scenario.Scenarios.TheRedhornGate (theRedhornGate) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheRedhornGate = TheRedhornGate ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRedhornGate :: Scenario
theRedhornGate = Scenario $ scenario TheRedhornGate "the-redhorn-gate" "The Redhorn Gate"

instance RunMessage TheRedhornGate where
  runMessage msg (TheRedhornGate attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheRedhornGate, EncounterSet.MistyMountains]
      firstStage <- genEncounterCard Quests.upThePass
      laterStages <- traverse genEncounterCard [Quests.snowdrifts, Quests.theMountainsPeaks]
      putQuestIntoPlay firstStage
      pure $ TheRedhornGate $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheRedhornGate <$> runMessage msg attrs
