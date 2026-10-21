module LordOfTheRings.Scenario.Scenarios.TheHuntForGollum (theHuntForGollum) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheHuntForGollum = TheHuntForGollum ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHuntForGollum :: Scenario
theHuntForGollum = Scenario $ scenario TheHuntForGollum "the-hunt-for-gollum" "The Hunt for Gollum"

instance RunMessage TheHuntForGollum where
  runMessage msg (TheHuntForGollum attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheHuntForGollum, EncounterSet.JourneyAlongTheAnduin, EncounterSet.SauronsReach]
      firstStage <- genEncounterCard Quests.theHuntBegins
      laterStages <- traverse genEncounterCard [Quests.aNewTerrorAbroad, Quests.onTheTrail]
      putQuestIntoPlay firstStage
      pure $ TheHuntForGollum $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheHuntForGollum <$> runMessage msg attrs
