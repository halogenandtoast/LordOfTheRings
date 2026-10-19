module LordOfTheRings.Scenario.Type (module LordOfTheRings.Scenario.Type) where

import LordOfTheRings.Card
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Type
import LordOfTheRings.Run

data Difficulty = Easy | Normal | Nightmare
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Show a, ToJSON a, Run a) => IsScenario a where
  scenarioAttrs :: a -> ScenarioAttrs
  default scenarioAttrs :: (Coercible a ScenarioAttrs) => a -> ScenarioAttrs
  scenarioAttrs = coerce

data Scenario where
  Scenario :: (IsScenario a) => a -> Scenario

instance HasField "phase" Scenario Phase where
  getField (Scenario a) = (scenarioAttrs a).phase

data ScenarioAttrs = ScenarioAttrs
  { questDeck :: [EncounterCard 'QuestKind]
  , quest :: Maybe Quest
  , encounterDeck :: [SomeEncounterCard]
  , difficulty :: Difficulty
  , phase :: Phase
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a
