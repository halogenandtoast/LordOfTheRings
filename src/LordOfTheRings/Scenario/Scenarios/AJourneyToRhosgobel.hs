module LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel (aJourneyToRhosgobel) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype AJourneyToRhosgobel = AJourneyToRhosgobel ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

aJourneyToRhosgobel :: Scenario
aJourneyToRhosgobel = Scenario $ scenario AJourneyToRhosgobel "a-journey-to-rhosgobel" "A Journey to Rhosgobel"

instance RunMessage AJourneyToRhosgobel where
  runMessage msg (AJourneyToRhosgobel attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.AJourneyToRhosgobel, EncounterSet.SpidersOfMirkwood, EncounterSet.Wilderlands]
      firstStage <- genEncounterCard Quests.theWoundedEagle
      laterStages <- traverse genEncounterCard [Quests.radagastsRequest, Quests.returnToRhosgobel]
      putQuestIntoPlay firstStage
      pure $ AJourneyToRhosgobel $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> AJourneyToRhosgobel <$> runMessage msg attrs
