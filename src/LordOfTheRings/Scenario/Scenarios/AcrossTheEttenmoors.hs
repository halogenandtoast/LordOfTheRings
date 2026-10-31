module LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors (acrossTheEttenmoors) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype AcrossTheEttenmoors = AcrossTheEttenmoors ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheEttenmoors :: Scenario
acrossTheEttenmoors = Scenario $ scenario AcrossTheEttenmoors "across-the-ettenmoors" "Across the Ettenmoors"

-- | The encounter deck is built from the Across the Ettenmoors, Eriador Wilds and Foul
-- Weather sets. The safe keyword's rules are kept by the safe locations themselves.
instance RunMessage AcrossTheEttenmoors where
  runMessage msg (AcrossTheEttenmoors attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.AcrossTheEttenmoors, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard Quests.intoTheEttenmoors
      laterStages <- traverse genEncounterCard [Quests.aMiserableJourney, Quests.journeysEnd]
      putQuestIntoPlay firstStage
      pure $ AcrossTheEttenmoors $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> AcrossTheEttenmoors <$> runMessage msg attrs
