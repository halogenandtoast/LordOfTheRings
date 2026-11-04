{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Scenario.Types (module LordOfTheRings.Scenario.Types) where

import LordOfTheRings.Campaign (CampaignLog)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.EncounterSet (EncounterSet (TheSiegeOfCairAndrosNightmare))
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Heading (Heading)
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Placement (GuardRelease)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

class
  ( Typeable a
  , Show a
  , ToJSON a
  , RunMessage a
  , HasAbilities a
  , HasModifiersFor a
  , Entity a
  , EntityId a ~ ScenarioId
  , EntityAttrs a ~ ScenarioAttrs
  )
  => IsScenario a

data Scenario = forall a. (IsScenario a) => Scenario a

-- | How hard the game is dealt: easy mode thins the encounter deck and gives each hero a
-- resource, nightmare mode builds the scenario's nightmare deck.
data GameMode = NormalMode | EasyMode | NightmareMode
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What a scenario's Nightmare Deck does to its encounter deck: the setup card, kept
-- flipped beside the quest deck, the standard cards it removes (by code, how many copies),
-- and the nightmare encounter set shuffled in.
data NightmareDeck = NightmareDeck
  { nightmareSetupCard :: CardCode
  , nightmareSet :: EncounterSet
  , nightmareRemovals :: [(CardCode, Int)]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON NightmareDeck where
  toJSON = genericToJSON (aesonOptions $ Just "nightmare")

instance FromJSON NightmareDeck where
  parseJSON = genericParseJSON (aesonOptions $ Just "nightmare")

-- | A nightmare deck: the setup card, the nightmare set, and each standard card removed
-- with the copies of it removed.
nightmareDeck :: CardDef -> EncounterSet -> [(CardDef, Int)] -> NightmareDeck
nightmareDeck setup set removals = NightmareDeck (toCardCode setup) set [(toCardCode d, n) | (d, n) <- removals]

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
  , scenarioMode :: GameMode
  , scenarioNightmareDeck :: Maybe NightmareDeck
  -- ^ static, from the scenario's definition: never read back from a saved game
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
  , scenarioOutOfPlayResources :: Map CardId Int
  -- ^ resources added to a card while it is out of play, placed on it as it enters play,
  -- as Captain's Quarters adds them to Captain Sahír
  , scenarioGuardedPlayerCards :: Map CardId [(GuardRelease, PlayerCard)]
  -- ^ the player cards each encounter card, by its card id, guards: a Guarded (X)
  -- attachment, or a card Hobgoblin holds (The Wilds of Rhovanion)
  , scenarioCavesDeck :: [EncounterCard]
  -- ^ the Caves deck, top first (Ered Mithrin): a Deep location traveled to is replaced
  -- by its top card
  , scenarioGainedVictoryPoints :: Map CardId Int
  -- ^ the victory points a card in the victory display gained as it went there, printed
  -- nowhere, as Fram's Shade's victory 6 (The Ghost of Framsburg)
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ScenarioAttrs

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "scenario")

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "scenario") . withDefaults [("setAside", toJSON ([] :: [EncounterCard])), ("orcGuards", object []), ("nightmareDeck", Null), ("meta", Null), ("hiddenCards", object []), ("heading", Null), ("corsairDeck", Null), ("corsairDiscard", toJSON ([] :: [EncounterCard])), ("unchartedDeck", toJSON ([] :: [EncounterCard])), ("grottoDeck", toJSON ([] :: [EncounterCard])), ("campaign", Null), ("campaignCardFlipped", toJSON False), ("outOfPlayResources", object []), ("guardedPlayerCards", object []), ("cavesDeck", toJSON ([] :: [EncounterCard])), ("gainedVictoryPoints", object [])] . withMode
   where
    -- a game saved before modes kept only whether it was easy
    withMode = \case
      Object o | not (KeyMap.member "mode" o) ->
        Object $ KeyMap.insert "mode" (toJSON $ if KeyMap.lookup "easyMode" o == Just (Bool True) then EasyMode else NormalMode) o
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
      , scenarioSetAside = []
      , scenarioOrcGuards = mempty
      , scenarioMode = NormalMode
      , scenarioNightmareDeck = Nothing
      , scenarioMeta = Null
      , scenarioHiddenCards = mempty
      , scenarioHeading = Nothing
      , scenarioCorsairDeck = Nothing
      , scenarioCorsairDiscard = []
      , scenarioUnchartedDeck = []
      , scenarioGrottoDeck = []
      , scenarioCampaign = Nothing
      , scenarioCampaignCardFlipped = False
      , scenarioOutOfPlayResources = mempty
      , scenarioGuardedPlayerCards = mempty
      , scenarioCavesDeck = []
      , scenarioGainedVictoryPoints = mempty
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

instance HasAbilities Scenario where
  getAbilities (Scenario a) = getAbilities a

instance HasModifiersFor Scenario where
  getModifiersFor (Scenario a) = getModifiersFor a

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

instance HasField "cavesDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioCavesDeck

instance HasField "questDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioQuestDeck

instance HasField "encounterDiscard" ScenarioAttrs [EncounterCard] where
  getField = scenarioEncounterDiscard

instance HasField "victoryDisplay" ScenarioAttrs [Card] where
  getField = scenarioVictoryDisplay

-- | The victory points of the cards in the victory display. The Siege of Cair Andros'
-- nightmare setup card: "player cards in the victory display do not contribute their
-- victory points."
instance HasField "victoryPoints" ScenarioAttrs Int where
  getField a = sum . mapMaybe points . filter counts $ scenarioVictoryDisplay a
   where
    -- The one setup card that touches scoring is named here: the score is read from the
    -- scenario alone, where no modifier can be seen.
    noPlayerCards = a.nightmare && fmap nightmareSet a.nightmareDeck == Just TheSiegeOfCairAndrosNightmare
    counts = \case
      PlayerCard _ -> not noPlayerCards
      _ -> True
    points c = cdVictoryPoints (toCardDef c) <|> Map.lookup c.id (scenarioGainedVictoryPoints a)

instance HasField "mode" ScenarioAttrs GameMode where
  getField = scenarioMode

instance HasField "easyMode" ScenarioAttrs Bool where
  getField = (== EasyMode) . scenarioMode

instance HasField "nightmare" ScenarioAttrs Bool where
  getField = (== NightmareMode) . scenarioMode

instance HasField "nightmareDeck" ScenarioAttrs (Maybe NightmareDeck) where
  getField = scenarioNightmareDeck

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
