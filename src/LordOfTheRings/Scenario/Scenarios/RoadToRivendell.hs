module LordOfTheRings.Scenario.Scenarios.RoadToRivendell (roadToRivendell) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype RoadToRivendell = RoadToRivendell ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

roadToRivendell :: Scenario
roadToRivendell = Scenario $ scenario RoadToRivendell "road-to-rivendell" "Road to Rivendell"

instance RunMessage RoadToRivendell where
  runMessage msg (RoadToRivendell attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.RoadToRivendell, EncounterSet.MistyMountains, EncounterSet.PlunderingGoblins]
      firstStage <- genEncounterCard Quests.alongTheMistyMountains
      laterStages <- traverse genEncounterCard [Quests.orcOutpost, Quests.approachingRivendell]
      putQuestIntoPlay firstStage
      pure $ RoadToRivendell $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> RoadToRivendell <$> runMessage msg attrs
