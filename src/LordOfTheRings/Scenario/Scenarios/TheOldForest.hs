module LordOfTheRings.Scenario.Scenarios.TheOldForest (theOldForest) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (forestStages)

newtype TheOldForest = TheOldForest ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldForest :: Scenario
theOldForest = Scenario $ scenario TheOldForest "the-old-forest" "The Old Forest"

instance RunMessage TheOldForest where
  runMessage msg (TheOldForest a) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets a [EncounterSet.TheOldForest]
      putQuestIntoPlay =<< genEncounterCard Quests.farewellToTheShire
      later <- traverse genEncounterCard (forestStages <> [Quests.theWickedWillow])
      pure $ TheOldForest $ a & encounterDeckL .~ deck & questDeckL .~ later
    _ -> TheOldForest <$> runMessage msg a
