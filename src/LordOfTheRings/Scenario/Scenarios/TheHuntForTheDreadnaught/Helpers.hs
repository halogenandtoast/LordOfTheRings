module LordOfTheRings.Scenario.Scenarios.TheHuntForTheDreadnaught.Helpers (initialGroupSize, globalPlayerCount) where

import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Epic.Types (EpicReplica (..), SharedEventState (..))
import LordOfTheRings.Game.Base (gamePlayerOrder, gameScenario)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioEpic)

-- The player order retains eliminated seats. Counts never shrink after elimination
-- (Dreadnaught rules, Global Values), unlike getRemainingPlayers.
initialGroupSize :: HasGame m => m Int
initialGroupSize = getsGame (length . gamePlayerOrder)

globalPlayerCount :: HasGame m => m Int
globalPlayerCount = do
  replica <- getsGame (scenarioEpic . toAttrs . gameScenario)
  maybe initialGroupSize (pure . totalPlayers . epicShared) replica
