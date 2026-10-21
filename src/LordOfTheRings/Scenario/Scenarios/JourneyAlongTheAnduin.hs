module LordOfTheRings.Scenario.Scenarios.JourneyAlongTheAnduin (journeyAlongTheAnduin) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype JourneyAlongTheAnduin = JourneyAlongTheAnduin ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeyAlongTheAnduin :: Scenario
journeyAlongTheAnduin = Scenario $ scenario JourneyAlongTheAnduin "journey-along-the-anduin" "Journey Along the Anduin"

instance RunMessage JourneyAlongTheAnduin where
  runMessage msg (JourneyAlongTheAnduin attrs) = case msg of
    -- the first stage is out from the start, its setup followed at step 7 as ever
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.JourneyAlongTheAnduin, EncounterSet.SauronsReach, EncounterSet.DolGuldurOrcs, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.toTheRiver
      laterStages <- traverse genEncounterCard [Quests.anduinPassage, Quests.ambushOnTheShore]
      putQuestIntoPlay firstStage
      pure $ JourneyAlongTheAnduin $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> JourneyAlongTheAnduin <$> runMessage msg attrs
