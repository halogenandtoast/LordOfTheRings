module LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood (returnToMirkwood) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype ReturnToMirkwood = ReturnToMirkwood ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

returnToMirkwood :: Scenario
returnToMirkwood = Scenario $ scenario ReturnToMirkwood "return-to-mirkwood" "Return to Mirkwood"

instance RunMessage ReturnToMirkwood where
  runMessage msg (ReturnToMirkwood attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.ReturnToMirkwood, EncounterSet.SpidersOfMirkwood, EncounterSet.Wilderlands, EncounterSet.DolGuldurOrcs]
      firstStage <- genEncounterCard Quests.throughTheForest
      laterStages <- traverse genEncounterCard [Quests.escapeAttempt, Quests.toTheElvenKingsHalls, Quests.ambush]
      putQuestIntoPlay firstStage
      pure $ ReturnToMirkwood $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> ReturnToMirkwood <$> runMessage msg attrs
