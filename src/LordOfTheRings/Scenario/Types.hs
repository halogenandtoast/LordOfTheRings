{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Scenario.Types (module LordOfTheRings.Scenario.Types) where

import LordOfTheRings.Campaign (CampaignLog)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.EncounterSet (EncounterSet (TheSiegeOfCairAndrosNightmare, Wilderlands))
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
  , scenarioEvilCreaturesDeck :: Maybe [EncounterCard]
  -- ^ the Evil Creatures deck, top first, while there is one: Wilderlands' enemies, taken
  -- out of the encounter deck (Journey Up the Anduin)
  , scenarioEvilCreaturesDiscard :: [EncounterCard]
  -- ^ the Evil Creatures deck's own discard pile
  , scenarioGainedVictoryPoints :: Map CardId Int
  -- ^ the victory points a card in the victory display gained as it went there, printed
  -- nowhere, as Fram's Shade's victory 6 (The Ghost of Framsburg)
  , scenarioPowerOfMordorDeck :: [EncounterCard]
  -- ^ the Power of Mordor deck, top first (The Temple of Doom, The Fortress of Nurn): The
  -- Power of Mordor encounter set, its cards revealed when a card says to
  , scenarioSackDeck :: [EncounterCard]
  -- ^ the sack deck, top first (We Must Away, Ere Break of Day): the Sack objectives,
  -- drawn by the first player as Sack X reads
  , scenarioBurdenDeck :: [EncounterCard]
  -- ^ the Burden deck, top first (Flight to the Ford): a burden set's cards, set next to
  -- the quest deck
  , scenarioInResolution :: Bool
  -- ^ the game won or lost, the campaign card's resolution under way, as Arkham's
  -- 'inResolution': the game ends once it has resolved
  , scenarioHeldReset :: Maybe [EncounterCard]
  -- ^ the discard pile, top first, shuffled into the encounter deck as the quest phase
  -- revealed the deck's last card, while that card's staging resolves: see 'HeldReset'
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''ScenarioAttrs

-- | A campaign uses its requested mode wherever a Nightmare Deck exists. Later
-- saga chapters without a published Nightmare Deck use the standard scenario.
campaignModeFor :: GameMode -> ScenarioAttrs -> GameMode
campaignModeFor NightmareMode attrs | isNothing attrs.scenarioNightmareDeck = NormalMode
campaignModeFor requested _ = requested

instance ToJSON ScenarioAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "scenario")

instance FromJSON ScenarioAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "scenario") . withDefaults [("setAside", toJSON ([] :: [EncounterCard])), ("orcGuards", object []), ("nightmareDeck", Null), ("meta", Null), ("hiddenCards", object []), ("heading", Null), ("corsairDeck", Null), ("corsairDiscard", toJSON ([] :: [EncounterCard])), ("unchartedDeck", toJSON ([] :: [EncounterCard])), ("grottoDeck", toJSON ([] :: [EncounterCard])), ("campaign", Null), ("campaignCardFlipped", toJSON False), ("outOfPlayResources", object []), ("guardedPlayerCards", object []), ("cavesDeck", toJSON ([] :: [EncounterCard])), ("evilCreaturesDeck", Null), ("evilCreaturesDiscard", toJSON ([] :: [EncounterCard])), ("gainedVictoryPoints", object []), ("powerOfMordorDeck", toJSON ([] :: [EncounterCard])), ("sackDeck", toJSON ([] :: [EncounterCard])), ("burdenDeck", toJSON ([] :: [EncounterCard])), ("inResolution", toJSON False), ("heldReset", Null)] . withMode
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
      , scenarioEvilCreaturesDeck = Nothing
      , scenarioEvilCreaturesDiscard = []
      , scenarioGainedVictoryPoints = mempty
      , scenarioPowerOfMordorDeck = []
      , scenarioSackDeck = []
      , scenarioBurdenDeck = []
      , scenarioInResolution = False
      , scenarioHeldReset = Nothing
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

instance HasField "evilCreaturesDeck" ScenarioAttrs (Maybe [EncounterCard]) where
  getField = scenarioEvilCreaturesDeck

instance HasField "powerOfMordorDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioPowerOfMordorDeck

instance HasField "sackDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioSackDeck

instance HasField "burdenDeck" ScenarioAttrs [EncounterCard] where
  getField = scenarioBurdenDeck

-- | A card of the Evil Creatures deck, while there is one: an enemy of the Wilderlands
-- encounter set (Journey Up the Anduin rules).
isEvilCreaturesCard :: ScenarioAttrs -> EncounterCard -> Bool
isEvilCreaturesCard a card = isJust a.evilCreaturesDeck && cdEncounterSet def == Just Wilderlands && cdCardType def == EnemyType
 where
  def = toCardDef card

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

instance HasField "inResolution" ScenarioAttrs Bool where
  getField = scenarioInResolution

instance HasField "heldReset" ScenarioAttrs (Maybe [EncounterCard]) where
  getField = scenarioHeldReset

{- | The reset of an encounter deck the quest phase emptied by revealing its last card
waits on that card's staging: the staging, When Revealed effects included, resolves
before the deck is reset, if able; if it has to use the encounter deck, the deck is reset
then and the effect finishes (FAQ v1.9, "When I reveal the last card of the encounter
deck..."; ALeP FAQ 2.73). The discard pile is shuffled in as the card is revealed, so an
effect that uses the deck finds it reset. Once the staging is done, or as it reveals
another card, the held reset settles:
-}
data HeldReset
  = -- | a card of the reset deck has left it: the deck was used, so reset as it was
    ResetUsed
  | -- | cards were added to the deck, none of the reset deck's taken, as Eastern Crows
    -- shuffles itself back in: the deck is not empty, so it is not reset, and holds only
    -- them (Caleb Grace, ALeP FAQ 3.110)
    ResetUndone [EncounterCard]
  | -- | nothing has used the deck: it is reset now, the cards discarded during the
    -- staging shuffled in too
    ResetPending

heldResetState :: ScenarioAttrs -> Maybe HeldReset
heldResetState a = settle <$> a.scenarioHeldReset
 where
  settle pile
    | any ((`notElem` deckIds) . (.id)) pile = ResetUsed
    | notNull added = ResetUndone added
    | otherwise = ResetPending
   where
    deckIds = map (.id) a.scenarioEncounterDeck
    added = filter ((`notElem` map (.id) pile) . (.id)) a.scenarioEncounterDeck

-- | The encounter deck as the held reset will leave it, unshuffled.
encounterDeckAfterHeldReset :: ScenarioAttrs -> [EncounterCard]
encounterDeckAfterHeldReset a = case heldResetState a of
  Just ResetPending -> a.scenarioEncounterDeck <> a.scenarioEncounterDiscard
  Just (ResetUndone added) -> added
  _ -> a.scenarioEncounterDeck
