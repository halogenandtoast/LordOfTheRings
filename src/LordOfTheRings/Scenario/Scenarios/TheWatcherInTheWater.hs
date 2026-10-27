module LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater (theWatcherInTheWater) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheWatcherInTheWater = TheWatcherInTheWater ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWatcherInTheWater :: Scenario
theWatcherInTheWater = Scenario $ scenario TheWatcherInTheWater "the-watcher-in-the-water" "The Watcher in the Water"

instance RunMessage TheWatcherInTheWater where
  runMessage msg (TheWatcherInTheWater attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheWatcherInTheWater, EncounterSet.MistyMountains]
      firstStage <- genEncounterCard Quests.toTheWestDoor
      laterStages <- traverse genEncounterCard [Quests.theSeethingLake]
      putQuestIntoPlay firstStage
      pure $ TheWatcherInTheWater $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheWatcherInTheWater <$> runMessage msg attrs
