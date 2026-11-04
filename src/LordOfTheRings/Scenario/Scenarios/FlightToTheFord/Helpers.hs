-- | What the cards of Flight to the Ford share: the Ring-bearer's life, kept by the scenario
-- in its meta on the life tracker (The Black Riders rules insert, "The Ring-bearer's Life").
module LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (
  getRingBearersLife,
  ringBearersLifeOf,
  setRingBearersLife,
  reduceRingBearersLife,
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

-- | The life the scenario's meta holds, 0 before the life tracker is set.
ringBearersLifeOf :: Value -> Int
ringBearersLifeOf = fromMaybe 0 . parseMaybe (withObject "meta" (.: "ringBearersLife"))

getRingBearersLife :: HasGame m => m Int
getRingBearersLife = getsGame (ringBearersLifeOf . scenarioMeta . toAttrs . gameScenario)

-- | "Set the Ring-bearer's life at 15", as stage 1A's setup reads.
setRingBearersLife :: ReverseQueue m => Int -> m ()
setRingBearersLife = push . ScenarioSpecific "flightToTheFord.setLife" . toJSON

-- | Reduce the Ring-bearer's life: at 0 the players lose the game. Nothing raises it again.
reduceRingBearersLife :: ReverseQueue m => Int -> m ()
reduceRingBearersLife n = when (n > 0) $ push $ ScenarioSpecific "flightToTheFord.reduceLife" (toJSON n)
