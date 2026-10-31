module LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador (theWastesOfEriador) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheWastesOfEriador = TheWastesOfEriador ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWastesOfEriador :: Scenario
theWastesOfEriador = Scenario $ scenario TheWastesOfEriador "the-wastes-of-eriador" "The Wastes of Eriador"

-- | Built from The Wastes of Eriador, Eriador Wilds and Foul Weather encounter sets. The
-- Time objective, Daybreak or Nightfall faceup, says whether it is Day or Night.
instance RunMessage TheWastesOfEriador where
  runMessage msg (TheWastesOfEriador attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheWastesOfEriador, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard Quests.acrossTheWastes
      laterStages <- traverse genEncounterCard [Quests.howlingAtNight, Quests.battleWithThePack]
      putQuestIntoPlay firstStage
      pure $ TheWastesOfEriador $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheWastesOfEriador <$> runMessage msg attrs
