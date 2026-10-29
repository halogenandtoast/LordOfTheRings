module LordOfTheRings.Scenario.Scenarios.TroubleInTharbad (troubleInTharbad) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TroubleInTharbad = TroubleInTharbad ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

troubleInTharbad :: Scenario
troubleInTharbad = Scenario $ scenario TroubleInTharbad "trouble-in-tharbad" "Trouble in Tharbad"

-- | The scenario keeps how many times Double Dealings has lowered each player's threat
-- elimination level, by 10 each, which the stages apply.
instance RunMessage TroubleInTharbad where
  runMessage msg (TroubleInTharbad attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TroubleInTharbad, EncounterSet.MistyMountainOrcs]
      firstStage <- genEncounterCard Quests.doubleDealings
      laterStages <- traverse genEncounterCard [Quests.escapeFromTharbad]
      putQuestIntoPlay firstStage
      pure $ TroubleInTharbad $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    ScenarioSpecific "tharbad.lowerElimination" _ -> do
      let lowered = fromMaybe (0 :: Int) $ parseMaybe parseJSON attrs.meta
      pure $ TroubleInTharbad $ attrs & metaL .~ toJSON (lowered + 1)
    _ -> TroubleInTharbad <$> runMessage msg attrs
