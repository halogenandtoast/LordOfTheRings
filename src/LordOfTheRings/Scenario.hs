{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario (module LordOfTheRings.Scenario, module X) where

import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id (ScenarioId)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Runner ()
import LordOfTheRings.Scenario.Scenarios.EscapeFromDolGuldur as X
import LordOfTheRings.Scenario.Scenarios.JourneyAlongTheAnduin as X
import LordOfTheRings.Scenario.Scenarios.PassageThroughMirkwood as X
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum as X
import LordOfTheRings.Scenario.Scenarios.ConflictAtTheCarrock as X
import LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel as X
import LordOfTheRings.Scenario.Scenarios.TheHillsOfEmynMuil as X
import LordOfTheRings.Scenario.Types as X

-- | The scenarios a game can be dealt, by id.
allScenarios :: [Scenario]
allScenarios = [passageThroughMirkwood, journeyAlongTheAnduin, escapeFromDolGuldur, theHuntForGollum, conflictAtTheCarrock, aJourneyToRhosgobel, theHillsOfEmynMuil]

lookupScenario :: ScenarioId -> Maybe Scenario
lookupScenario sid = find ((== sid) . toId) allScenarios

instance FromJSON Scenario where
  parseJSON v = do
    attrs <- parseJSON @ScenarioAttrs v
    case lookupScenario (scenarioId attrs) of
      Just s -> pure $ overAttrs (const attrs) s
      Nothing -> fail $ "Unknown scenario: " <> show (scenarioId attrs)

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a
