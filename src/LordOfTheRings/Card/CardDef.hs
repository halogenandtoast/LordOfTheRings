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

data CardCost = StaticCost Int | DynamicCost
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
  , -- | revealing it has a When Revealed effect, which the lead resolves by clicking it
    cdWhenRevealed :: Bool
  -- ^ an encounter card with a shadow effect, as opposed to a blank shadow card
  , cdDistinctPayers :: Maybe Int
  -- ^ the card must be paid for from this many different heroes' pools
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CardDef where
  toJSON = genericToJSON (aesonOptions $ Just "cd")

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
    , cdWhenRevealed = False
    , cdDistinctPayers = Nothing
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

event :: CardCode -> Name -> Sphere -> CardCost -> CardDef
event cCode name sphere cost = (playerCard cCode name EventType sphere) {cdCost = Just cost}

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

objective :: CardCode -> Name -> EncounterSet -> Int -> CardDef
objective cCode name encounterSet quantity = encounterCard cCode name ObjectiveType encounterSet quantity

isPlayerCardDef :: CardDef -> Bool
isPlayerCardDef = isPlayerCardType . cdCardType
