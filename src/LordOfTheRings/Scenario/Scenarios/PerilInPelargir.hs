module LordOfTheRings.Scenario.Scenarios.PerilInPelargir (perilInPelargir) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype PerilInPelargir = PerilInPelargir ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

perilInPelargir :: Scenario
perilInPelargir = Scenario $ scenario PerilInPelargir "peril-in-pelargir" "Peril in Pelargir"

instance RunMessage PerilInPelargir where
  runMessage msg (PerilInPelargir attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.PerilInPelargir, EncounterSet.StreetsOfGondor, EncounterSet.Brigands]
      firstStage <- genEncounterCard Quests.theLeapingFishQuest
      laterStages <- traverse genEncounterCard [Quests.fightingInTheStreets, Quests.escapeToTheQuays]
      putQuestIntoPlay firstStage
      pure $ PerilInPelargir $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> PerilInPelargir <$> runMessage msg attrs
