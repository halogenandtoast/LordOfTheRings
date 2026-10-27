module LordOfTheRings.Scenario.Scenarios.TheSeventhLevel (theSeventhLevel) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheSeventhLevel = TheSeventhLevel ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSeventhLevel :: Scenario
theSeventhLevel = Scenario $ scenario TheSeventhLevel "the-seventh-level" "The Seventh Level"

instance RunMessage TheSeventhLevel where
  runMessage msg (TheSeventhLevel attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheSeventhLevel, EncounterSet.PlunderingGoblins, EncounterSet.GoblinsOfTheDeep]
      firstStage <- genEncounterCard Quests.searchForTheChamber
      laterStages <- traverse genEncounterCard [Quests.theFateOfBalin]
      putQuestIntoPlay firstStage
      pure $ TheSeventhLevel $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheSeventhLevel <$> runMessage msg attrs
