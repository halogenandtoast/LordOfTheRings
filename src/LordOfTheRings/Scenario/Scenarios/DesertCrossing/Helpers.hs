-- | What the cards of Desert Crossing share: the heat tracker's temperature, kept by the
-- scenario in its meta.
module LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (
  getTemperature,
  temperatureX,
  temperatureOf,
  increaseTemperature,
  reduceTemperature,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Message (Message (ScenarioSpecific))
import LordOfTheRings.Message.Lifted (ReverseQueue)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioMeta)

-- | The temperature the scenario's meta holds, 0 before the heat tracker is set.
temperatureOf :: Value -> Int
temperatureOf = fromMaybe 0 . parseMaybe (withObject "meta" (.: "temperature"))

getTemperature :: HasGame m => m Int
getTemperature = getsGame (temperatureOf . scenarioMeta . toAttrs . gameScenario)

-- | "X is the tens digit of the temperature."
temperatureX :: HasGame m => m Int
temperatureX = (`div` 10) <$> getTemperature

-- | The scenario moves the heat tracker by this much; at 60 the players lose.
increaseTemperature :: ReverseQueue m => Int -> m ()
increaseTemperature = push . ScenarioSpecific "desertCrossing.temperature" . toJSON

reduceTemperature :: ReverseQueue m => Int -> m ()
reduceTemperature = increaseTemperature . negate
