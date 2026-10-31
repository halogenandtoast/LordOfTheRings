module LordOfTheRings.Scenario.Scenarios.TheDruadanForest (theDruadanForest) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheDruadanForest = TheDruadanForest ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDruadanForest :: Scenario
theDruadanForest = Scenario $ scenario TheDruadanForest "the-druadan-forest" "The Drúadan Forest"

instance RunMessage TheDruadanForest where
  runMessage msg (TheDruadanForest attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheDruadanForest, EncounterSet.BroodingForest]
      firstStage <- genEncounterCard Quests.thePursuit
      laterStages <- traverse genEncounterCard [Quests.anUntimelyEnd, Quests.thePassageOut]
      putQuestIntoPlay firstStage
      pure $ TheDruadanForest $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheDruadanForest <$> runMessage msg attrs
