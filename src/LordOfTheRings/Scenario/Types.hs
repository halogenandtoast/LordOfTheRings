{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Scenario.Types (module LordOfTheRings.Scenario.Types) where

import LordOfTheRings.Campaign (CampaignLog)
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Heading (Heading)
import LordOfTheRings.Id
import LordOfTheRings.Json
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
  , -- | encounter cards, and player cards an effect adds, as The Red Arrow
    scenarioVictoryDisplay :: [Card]
  , scenarioSetAside :: [EncounterCard]
  -- ^ out of play until the scenario calls for them
  , scenarioOrcGuards :: Map CardId PlayerCard
  -- ^ the player card face down under each Orc Guard
  , scenarioEasyMode :: Bool
  -- ^ fewer copies of the difficult encounter cards, and a resource more for each hero
  , scenarioMeta :: Value
  -- ^ what only one scenario keeps, as Arkham's meta: its module reads and writes it
  , scenarioHiddenCards :: Map PlayerId [EncounterCard]
  -- ^ the encounter cards facedown in each player's play area (The Blood of Gondor)
  , scenarioHeading :: Maybe Heading
  -- ^ the Dream-chaser's heading, once the players have prepared their fleet
  , scenarioCorsairDeck :: Maybe [EncounterCard]
  -- ^ the Corsair deck, top first, once prepared: the non-Ship enemies taken out of the
  -- encounter deck (Dream-chaser rules)
  , scenarioCorsairDiscard :: [EncounterCard]
  -- ^ the Corsair deck's own discard pile
  , scenarioUnchartedDeck :: [EncounterCard]
  -- ^ the Uncharted deck, top first (The Fate of Númenor): its locations enter play Lost
  -- Island side faceup, and return to it rather than the discard pile
  , scenarioGrottoDeck :: [EncounterCard]
  -- ^ the Grotto deck, top first (The Drowned Ruins): double-sided locations, Grotto side
  -- up, an explored Grotto side shuffled back into it
  , scenarioCampaign :: Maybe CampaignLog
  -- ^ in campaign mode, the campaign log as the scenario began, then as it is resolved
  , scenarioCampaignCardFlipped :: Bool
  -- ^ the campaign card turned over, as its resolution is read, or Escape from Dol
  -- Guldur's when the prisoner is taken
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ScenarioAttrs

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "scenario")

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "scenario") . withDefaults [("setAside", toJSON ([] :: [EncounterCard])), ("orcGuards", object []), ("easyMode", toJSON False), ("meta", Null), ("hiddenCards", object []), ("heading", Null), ("corsairDeck", Null), ("corsairDiscard", toJSON ([] :: [EncounterCard])), ("unchartedDeck", toJSON ([] :: [EncounterCard])), ("grottoDeck", toJSON ([] :: [EncounterCard])), ("campaign", Null), ("campaignCardFlipped", toJSON False)]

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
      , scenarioSetAside = []
      , scenarioOrcGuards = mempty
      , scenarioEasyMode = False
      , scenarioMeta = Null
      , scenarioHiddenCards = mempty
      , scenarioHeading = Nothing
      , scenarioCorsairDeck = Nothing
      , scenarioCorsairDiscard = []
      , scenarioUnchartedDeck = []
      , scenarioGrottoDeck = []
      , scenarioCampaign = Nothing
      , scenarioCampaignCardFlipped = False
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

instance HasField "victoryDisplay" ScenarioAttrs [Card] where
  getField = scenarioVictoryDisplay

-- | The victory points of the cards in the victory display.
instance HasField "victoryPoints" ScenarioAttrs Int where
  getField = sum . mapMaybe (cdVictoryPoints . toCardDef) . scenarioVictoryDisplay

instance HasField "easyMode" ScenarioAttrs Bool where
  getField = scenarioEasyMode

instance HasField "meta" ScenarioAttrs Value where
  getField = scenarioMeta

instance HasField "hiddenCards" ScenarioAttrs (Map PlayerId [EncounterCard]) where
  getField = scenarioHiddenCards

instance HasField "heading" ScenarioAttrs (Maybe Heading) where
  getField = scenarioHeading

instance HasField "corsairDeck" ScenarioAttrs (Maybe [EncounterCard]) where
  getField = scenarioCorsairDeck

instance HasField "corsairDiscard" ScenarioAttrs [EncounterCard] where
  getField = scenarioCorsairDiscard
instance HasField "unchartedDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioUnchartedDeck

instance HasField "grottoDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioGrottoDeck

instance HasField "campaign" ScenarioAttrs (Maybe CampaignLog) where
  getField = scenarioCampaign
