module LordOfTheRings.Card.CardDef (module LordOfTheRings.Card.CardDef) where

import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Json
import LordOfTheRings.Keyword
import LordOfTheRings.Name
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

-- | A printed X is 'DynamicCost' when the player chooses it as the card is paid for, or
-- 'XCost' when the card's own text sets it, as The White Council's number of players: it
-- is paid as 0 raised by the card's modifiers. Out of play, either X is 0.
data CardCost = StaticCost Int | DynamicCost | XCost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What a player card asks besides its resources to be played from hand, as Arkham's
-- additional costs.
data AdditionalCost = DiscardCardsFromHand Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The racing test result printed in an encounter card's lower right corner (MEC80
-- rules, "Racing X Tests"): a WILD result counts as both a STRAIGHT and a TURN result.
data RacingResult = Straight | Turn | Wild
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | What a riddle printed on an encounter card has the first player name (The Hobbit: Over
Hill and Under Hill rules insert, "Riddles"): a card type (ally, event, attachment or
treasure), a sphere, a cost. A discarded card matches if it has each item named.
-}
data RiddleItem = RiddleCardType | RiddleSphere | RiddleCost
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A riddle: the items named, and how many cards the first player discards from the top
-- of his deck to answer it.
data Riddle = Riddle {riddleItems :: [RiddleItem], riddleDiscards :: Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Where a player card may be played from.
data PlayableFrom = FromHand | FromHandOrDiscard | FromDiscardOnly
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CardDef = CardDef
  { cdCardCode :: CardCode
  , cdName :: Name
  , cdCardType :: CardType
  , cdSphere :: Maybe Sphere
  , cdUnique :: Bool
  , cdCost :: Maybe CardCost
  , cdThreatCost :: Maybe Int
  , cdWillpower :: Maybe Int
  , cdAttack :: Maybe Int
  , cdDefense :: Maybe Int
  , cdHitPoints :: Maybe Int
  , cdThreat :: Maybe Int
  , cdEngagementCost :: Maybe Int
  , cdQuestPoints :: Maybe Int
  , cdStage :: Maybe Int
  , cdVictoryPoints :: Maybe Int
  , cdCardTraits :: Set Trait
  , cdKeywords :: Set Keyword
  , cdEncounterSet :: Maybe EncounterSet
  , cdEncounterSetQuantity :: Maybe Int
  , cdEasyModeQuantity :: Maybe Int
  -- ^ the copies kept in easy mode, when it keeps fewer than all of them
  , cdDoubleSided :: Bool
  , cdShadowEffect :: Bool
  , -- | its shadow effect cannot be canceled, as Blazing Grip's reads
    cdShadowCannotBeCanceled :: Bool
  , -- | revealing it has a When Revealed effect, which the lead resolves by clicking it
    cdWhenRevealed :: Bool
  -- ^ an encounter card with a shadow effect, as opposed to a blank shadow card
  , cdDistinctPayers :: Maybe Int
  -- ^ the card must be paid for from this many different heroes' pools
  , cdSinglePayer :: Bool
  -- ^ the card's cost must be paid from a single hero's pool, as Knight of the White Tower's
  , cdEscape :: Maybe Int
  -- ^ what the card adds to an escape test it is dealt for
  , cdSailingIcons :: Int
  -- ^ the sailing success icons printed on the encounter card, found by a Sailing test
  , cdRacingResult :: Maybe RacingResult
  -- ^ the racing test result printed on the encounter card, found by a racing test
  , cdRiddle :: Maybe Riddle
  -- ^ the riddle printed on the encounter card, as Wild Wargs' (Dungeons Deep and Caverns Dim)
  , cdDeckLimit :: Int
  -- ^ the copies of its title a deck may hold
  , cdLimitOnePerHost :: Bool
  -- ^ an attachment limited to 1 per character: no host takes a second copy of its title
  , cdLimitOneInVictoryDisplay :: Bool
  -- ^ a side quest of which only 1 copy enters the victory display: another copy defeated
  -- goes to its owner's discard pile (The Sands of Harad rules)
  , cdPlayableFrom :: PlayableFrom
  -- ^ where its owner may play it from, as Glorfindel can be played from the discard pile
  , cdOtherSide :: Maybe CardCode
  -- ^ a double-sided card's other side, as Captain Sahír's objective-ally side, or an
  -- Uncharted location's Lost Island side
  , cdAdditionalCost :: Maybe AdditionalCost
  -- ^ what playing it from hand costs besides its resources, as Guardian of Rivendell
  , cdRestrictedSlots :: Int
  -- ^ how many of a character's 2 restricted attachments a restricted card counts as, as
  -- Raiment of War counts as 2
  , cdEncounterToOwnersDiscard :: Bool
  -- ^ a player card with the Encounter keyword that goes to its owner's discard pile, not
  -- the encounter discard pile, as Ranger of the North (The Lost Realm rules); one without
  -- is treated as an encounter card there (MEC61, MEC67 rules)
  , cdSetIcon :: Maybe EncounterSet
  -- ^ a saga boon, burden or treasure's set icon, in place of an encounter set: it is never
  -- gathered with one. The Lord of the Rings' campaign cards remove the burdens of the sets
  -- they name; a Hobbit scenario lets in the treasures of the sets its setup lists
  , cdTreasure :: Bool
  , cdSetupAttach :: Bool
  -- ^ a player card reading "Setup: Attach to a hero in play", as The Road Darkens' Gift
  -- boons: in its owner's deck as the game is set up, it is attached before the setup hand
  -- is drawn (The Black Riders rules insert, "New Setup Instructions")
  -- ^ a Hobbit saga treasure: a player card discovered in one scenario and added to its
  -- player's deck in a later one (The Hobbit: Over Hill and Under Hill rules insert)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CardDef where
  toJSON = genericToJSON (aesonOptions $ Just "cd")

instance HasField "traits" CardDef (Set Trait) where
  getField = cdCardTraits

class HasCardDef a where
  toCardDef :: a -> CardDef

instance HasCardDef CardDef where
  toCardDef = id

instance HasCardCode CardDef where
  toCardCode = cdCardCode

emptyCardDef :: CardCode -> Name -> CardType -> CardDef
emptyCardDef cCode name cType =
  CardDef
    { cdCardCode = cCode
    , cdName = name
    , cdCardType = cType
    , cdSphere = Nothing
    , cdUnique = False
    , cdCost = Nothing
    , cdThreatCost = Nothing
    , cdWillpower = Nothing
    , cdAttack = Nothing
    , cdDefense = Nothing
    , cdHitPoints = Nothing
    , cdThreat = Nothing
    , cdEngagementCost = Nothing
    , cdQuestPoints = Nothing
    , cdStage = Nothing
    , cdVictoryPoints = Nothing
    , cdCardTraits = mempty
    , cdKeywords = mempty
    , cdEncounterSet = Nothing
    , cdEncounterSetQuantity = Nothing
    , cdEasyModeQuantity = Nothing
    , cdDoubleSided = False
    , cdShadowEffect = False
    , cdShadowCannotBeCanceled = False
    , cdWhenRevealed = False
    , cdDistinctPayers = Nothing
    , cdSinglePayer = False
    , cdEscape = Nothing
    , cdSailingIcons = 0
    , cdRacingResult = Nothing
    , cdRiddle = Nothing
    , cdDeckLimit = 3
    , cdLimitOnePerHost = False
    , cdLimitOneInVictoryDisplay = False
    , cdPlayableFrom = FromHand
    , cdOtherSide = Nothing
    , cdAdditionalCost = Nothing
    , cdRestrictedSlots = 1
    , cdEncounterToOwnersDiscard = False
    , cdSetIcon = Nothing
    , cdTreasure = False
    , cdSetupAttach = False
    }

unique :: CardDef -> CardDef
unique def = def {cdUnique = True}

playerCard :: CardCode -> Name -> CardType -> Sphere -> CardDef
playerCard cCode name cType sphere = (emptyCardDef cCode name cType) {cdSphere = Just sphere}

hero :: CardCode -> Name -> Sphere -> Int -> Int -> Int -> Int -> Int -> CardDef
hero cCode name sphere threatCost willpower attack defense hitPoints =
  (playerCard cCode name HeroType sphere)
    { cdUnique = True
    , cdThreatCost = Just threatCost
    , cdWillpower = Just willpower
    , cdAttack = Just attack
    , cdDefense = Just defense
    , cdHitPoints = Just hitPoints
    }

ally :: CardCode -> Name -> Sphere -> CardCost -> Int -> Int -> Int -> Int -> CardDef
ally cCode name sphere cost willpower attack defense hitPoints =
  (playerCard cCode name AllyType sphere)
    { cdCost = Just cost
    , cdWillpower = Just willpower
    , cdAttack = Just attack
    , cdDefense = Just defense
    , cdHitPoints = Just hitPoints
    }

attachment :: CardCode -> Name -> Sphere -> CardCost -> CardDef
attachment cCode name sphere cost = (playerCard cCode name AttachmentType sphere) {cdCost = Just cost}

playerSideQuest :: CardCode -> Name -> Sphere -> CardCost -> Int -> CardDef
playerSideQuest cCode name sphere cost questPoints =
  (playerCard cCode name PlayerSideQuestType sphere) {cdCost = Just cost, cdQuestPoints = Just questPoints}

event :: CardCode -> Name -> Sphere -> CardCost -> CardDef
event cCode name sphere cost = (playerCard cCode name EventType sphere) {cdCost = Just cost}

-- | A contract: no sphere, cost or deck slot, one to a player (MEC77 rules, "Contracts").
contract :: CardCode -> Name -> CardDef
contract cCode name = (emptyCardDef cCode name ContractType) {cdDeckLimit = 1}

encounterCard :: CardCode -> Name -> CardType -> EncounterSet -> Int -> CardDef
encounterCard cCode name cType encounterSet quantity =
  (emptyCardDef cCode name cType)
    { cdEncounterSet = Just encounterSet
    , cdEncounterSetQuantity = Just quantity
    }

enemy :: CardCode -> Name -> EncounterSet -> Int -> Int -> Int -> Int -> Int -> Int -> CardDef
enemy cCode name encounterSet quantity engagementCost threat attack defense hitPoints =
  (encounterCard cCode name EnemyType encounterSet quantity)
    { cdEngagementCost = Just engagementCost
    , cdThreat = Just threat
    , cdAttack = Just attack
    , cdDefense = Just defense
    , cdHitPoints = Just hitPoints
    }

location :: CardCode -> Name -> EncounterSet -> Int -> Int -> Int -> CardDef
location cCode name encounterSet quantity threat questPoints =
  (encounterCard cCode name LocationType encounterSet quantity)
    { cdThreat = Just threat
    , cdQuestPoints = Just questPoints
    }

treachery :: CardCode -> Name -> EncounterSet -> Int -> CardDef
treachery cCode name encounterSet quantity = (encounterCard cCode name TreacheryType encounterSet quantity) {cdWhenRevealed = True}

quest :: CardCode -> Name -> EncounterSet -> Int -> Int -> CardDef
quest cCode name encounterSet stage questPoints =
  (encounterCard cCode name QuestType encounterSet 1)
    { cdStage = Just stage
    , cdQuestPoints = Just questPoints
    , cdDoubleSided = True
    }

-- | An encounter side quest, printed as "Side Quest", or as a quest card with the Side Quest
-- trait: one copy, single-sided. Its victory points, if any, go on with a record update.
encounterSideQuest :: CardCode -> Name -> EncounterSet -> Int -> CardDef
encounterSideQuest cCode name encounterSet questPoints =
  (encounterCard cCode name EncounterSideQuestType encounterSet 1) {cdQuestPoints = Just questPoints}

objective :: CardCode -> Name -> EncounterSet -> Int -> CardDef
objective cCode name encounterSet quantity = encounterCard cCode name ObjectiveType encounterSet quantity

-- | A Nightmare Deck's setup card: never in the encounter deck, its text the scenario's.
nightmareSetup :: CardCode -> Name -> EncounterSet -> CardDef
nightmareSetup cCode name encounterSet = encounterCard cCode name NightmareType encounterSet 1

isPlayerCardDef :: CardDef -> Bool
isPlayerCardDef = isPlayerCardType . cdCardType
