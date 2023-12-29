module LordOfTheRings.Scenario.Type (module LordOfTheRings.Scenario.Type) where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Type
import LordOfTheRings.Run

data Difficulty = Easy | Normal | Nightmare
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Show a, ToJSON a, Run a) => IsScenario a

data Scenario where
  Scenario :: (IsScenario a) => a -> Scenario

data ScenarioAttrs = ScenarioAttrs
  { questDeck :: [EncounterCard 'QuestKind]
  , quest :: Maybe Quest
  , encounterDeck :: [SomeEncounterCard]
  , difficulty :: Difficulty
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a
