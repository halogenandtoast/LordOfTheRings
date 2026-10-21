module LordOfTheRings.Scenario.Scenarios.TheHillsOfEmynMuil (theHillsOfEmynMuil) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheHillsOfEmynMuil = TheHillsOfEmynMuil ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHillsOfEmynMuil :: Scenario
theHillsOfEmynMuil = Scenario $ scenario TheHillsOfEmynMuil "the-hills-of-emyn-muil" "The Hills of Emyn Muil"

instance RunMessage TheHillsOfEmynMuil where
  runMessage msg (TheHillsOfEmynMuil attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- gatherEncounterSet attrs EncounterSet.TheHillsOfEmynMuil
      putQuestIntoPlay =<< genEncounterCard Quests.theHillsOfEmynMuil
      pure $ TheHillsOfEmynMuil $ attrs & encounterDeckL .~ encounterDeck
    _ -> TheHillsOfEmynMuil <$> runMessage msg attrs
