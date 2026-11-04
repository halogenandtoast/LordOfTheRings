module LordOfTheRings.Scenario.Scenarios.DesertCrossing (desertCrossing) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (temperatureOf)

newtype DesertCrossing = DesertCrossing ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

desertCrossing :: Scenario
desertCrossing = Scenario $ scenario DesertCrossing "desert-crossing" "Desert Crossing"

{- | The encounter deck is built from Desert Crossing, Desert Creatures and Desert Sands.
The heat tracker is the scenario's: its temperature is kept in the meta, set to 10 as
stage 1A's setup reads, and moved by 'increaseTemperature'. "If the temperature reaches
60, the players lose the game" (MEC55 rules, Heat Tracker): checked as it moves, which
covers each stage's own "if the temperature is 60 or higher" too.
-}
instance RunMessage DesertCrossing where
  runMessage msg (DesertCrossing attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.DesertCrossing, EncounterSet.DesertCreatures, EncounterSet.DesertSands]
      firstStage <- genEncounterCard Quests.theDesertOfHarad
      laterStages <- traverse genEncounterCard [Quests.underTheMercilessSun, Quests.beastsOfLegend]
      putQuestIntoPlay firstStage
      pure
        $ DesertCrossing
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & metaL .~ object ["temperature" .= (10 :: Int)]
    ScenarioSpecific "desertCrossing.temperature" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      let t = max 0 (temperatureOf attrs.meta + n)
      when (t >= 60) $ gameOver Lost
      pure $ DesertCrossing $ attrs & metaL .~ object ["temperature" .= t]
    _ -> DesertCrossing <$> runMessage msg attrs
