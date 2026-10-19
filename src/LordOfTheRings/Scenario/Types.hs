{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Scenario.Types (module LordOfTheRings.Scenario.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id
import LordOfTheRings.Json
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

class
  ( Typeable a
  , Show a
  , ToJSON a
  , RunMessage a
  , Entity a
  , EntityId a ~ ScenarioId
  , EntityAttrs a ~ ScenarioAttrs
  )
  => IsScenario a

data Scenario = forall a. (IsScenario a) => Scenario a

data ScenarioAttrs = ScenarioAttrs
  { scenarioId :: ScenarioId
  , scenarioName :: Text
  , scenarioEncounterDeck :: [EncounterCard]
  , scenarioEncounterDiscard :: [EncounterCard]
  , scenarioQuestDeck :: [EncounterCard]
  , scenarioVictoryDisplay :: [EncounterCard]
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ScenarioAttrs

-- | The encounter and quest decks are hidden information, so only their sizes are sent.
instance ToJSON ScenarioAttrs where
  toJSON a = case genericToJSON (aesonOptions $ Just "scenario") a of
    Object o ->
      Object
        $ KeyMap.insert "encounterDeckSize" (toJSON $ length a.scenarioEncounterDeck)
        $ KeyMap.insert "questDeckSize" (toJSON $ length a.scenarioQuestDeck)
        $ KeyMap.delete "encounterDeck"
        $ KeyMap.delete "questDeck" o
    v -> v

scenario :: (ScenarioAttrs -> a) -> ScenarioId -> Text -> a
scenario f sid name =
  f
    ScenarioAttrs
      { scenarioId = sid
      , scenarioName = name
      , scenarioEncounterDeck = []
      , scenarioEncounterDiscard = []
      , scenarioQuestDeck = []
      , scenarioVictoryDisplay = []
      }

instance Entity ScenarioAttrs where
  type EntityId ScenarioAttrs = ScenarioId
  type EntityAttrs ScenarioAttrs = ScenarioAttrs
  toId = scenarioId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Scenario where
  type EntityId Scenario = ScenarioId
  type EntityAttrs Scenario = ScenarioAttrs
  toId = toId . toAttrs
  toAttrs (Scenario a) = toAttrs a
  overAttrs f (Scenario a) = Scenario $ overAttrs f a

instance Show Scenario where
  show (Scenario a) = show a

instance ToJSON Scenario where
  toJSON (Scenario a) = toJSON a

instance Targetable ScenarioAttrs where
  toTarget _ = ScenarioTarget

instance Sourceable ScenarioAttrs where
  toSource _ = ScenarioSource

instance HasField "encounterDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioEncounterDeck

instance HasField "questDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioQuestDeck

instance HasField "encounterDiscard" ScenarioAttrs [EncounterCard] where
  getField = scenarioEncounterDiscard
