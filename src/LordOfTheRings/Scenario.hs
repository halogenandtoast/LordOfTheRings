{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario (module LordOfTheRings.Scenario, module X) where

import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Runner ()
import LordOfTheRings.Scenario.Scenarios.PassageThroughMirkwood as X
import LordOfTheRings.Scenario.Types as X

instance FromJSON Scenario where
  parseJSON v = do
    attrs <- parseJSON @ScenarioAttrs v
    case scenarioId attrs of
      "passage-through-mirkwood" -> pure $ overAttrs (const attrs) passageThroughMirkwood
      other -> fail $ "Unknown scenario: " <> show other

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a
