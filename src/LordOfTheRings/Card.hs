{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Card (module LordOfTheRings.Card) where

import Control.Monad.Random
import Data.Aeson
import Data.String (IsString (..))
import GHC.Generics
import GHC.Records
import LordOfTheRings.EncounterSet
import LordOfTheRings.Id
import LordOfTheRings.Prelude

class Display a where
  display :: a -> Text

instance Display Name where
  display (Name title mSubtitle) = case mSubtitle of
    Nothing -> title
    Just subtitle -> title <> ": " <> subtitle

data CardKind = HeroKind | AllyKind | AttachmentKind | EventKind
data EncounterCardKind = QuestKind | EnemyKind | TreacheryKind | LocationKind

data SCardKind k where
  SHeroKind :: SCardKind 'HeroKind
  SAllyKind :: SCardKind 'AllyKind
  SAttachmentKind :: SCardKind 'AttachmentKind
  SEventKind :: SCardKind 'EventKind

data SEncounterCardKind k where
  SQuestKind :: SEncounterCardKind 'QuestKind
  SEnemyKind :: SEncounterCardKind 'EnemyKind
  STreacheryKind :: SEncounterCardKind 'TreacheryKind
  SLocationKind :: SEncounterCardKind 'LocationKind

data SomeSEncounterCardKind where
  SomeSEncounterCardKind :: SEncounterCardKind k -> SomeSEncounterCardKind

deriving stock instance Show (SCardKind k)
deriving stock instance Show (SEncounterCardKind k)

instance ToJSON (SCardKind k) where
  toJSON = \case
    SHeroKind -> "hero"
    SAllyKind -> "ally"
    SAttachmentKind -> "attachment"
    SEventKind -> "event"

instance ToJSON (SEncounterCardKind k) where
  toJSON = \case
    SQuestKind -> "quest"
    SEnemyKind -> "enemy"
    STreacheryKind -> "treachery"
    SLocationKind -> "location"

instance FromJSON SomeSEncounterCardKind where
  parseJSON = withText "SomeSEncounterCardKind" $ \case
    "quest" -> pure $ SomeSEncounterCardKind SQuestKind
    "enemy" -> pure $ SomeSEncounterCardKind SEnemyKind
    "treachery" -> pure $ SomeSEncounterCardKind STreacheryKind
    "location" -> pure $ SomeSEncounterCardKind SLocationKind
    _ -> fail "Invalid EncounterCardKind"

newtype CardCode = CardCode Text
  deriving newtype (Ord, Eq, IsString, Show, ToJSON, FromJSON)

data Card (k :: CardKind) = Card
  { id :: CardId
  , def :: CardDef k
  , owner :: Maybe PlayerId
  }

data EncounterCard (k :: EncounterCardKind) = EncounterCard
  { id :: CardId
  , def :: EncounterCardDef k
  }

instance FromJSON (SCardKind AllyKind) where
  parseJSON = withText "CardKind" $ \case
    "ally" -> pure SAllyKind
    _ -> fail "Invalid CardKind"

deriving stock instance Generic (Card 'AllyKind)
deriving stock instance Generic (CardDef 'AllyKind)
deriving anyclass instance FromJSON (CardDef 'AllyKind)
deriving anyclass instance FromJSON (Card 'AllyKind)

instance HasField "code" (Card k) CardCode where
  getField card = card.def.code

instance HasField "name" (Card k) Name where
  getField card = card.def.name

deriving stock instance (Show (CardDetails k)) => Show (Card k)

instance (ToJSON (CardDef k)) => ToJSON (Card k) where
  toJSON (Card cid def owner) = object ["id" .= toJSON cid, "def" .= toJSON def, "owner" .= toJSON owner]

data SomeEncounterCard where
  SomeEncounterCard
    :: (Show (EncounterCardDef k), Show (EncounterCardDetails k), ToJSON (EncounterCard k))
    => EncounterCard k
    -> SomeEncounterCard

instance Show SomeEncounterCard where
  show (SomeEncounterCard card) = show card

instance ToJSON SomeEncounterCard where
  toJSON (SomeEncounterCard card) = toJSON card

data SomeCard where
  SomeCard :: (Show (CardDef k), Show (CardDetails k), ToJSON (Card k)) => Card k -> SomeCard

instance HasField "unique" SomeCard Bool where
  getField (SomeCard card) = card.def.unique

instance HasField "name" SomeCard Name where
  getField (SomeCard card) = card.def.name

instance HasField "code" SomeCard CardCode where
  getField (SomeCard card) = card.def.code

instance HasField "sphere" SomeCard Sphere where
  getField (SomeCard card) = card.def.sphere

instance HasField "cost" SomeCard (Maybe ResourceCost) where
  getField = cardCost

instance HasField "id" SomeCard CardId where
  getField (SomeCard card) = card.id

instance HasField "id" SomeEncounterCard CardId where
  getField (SomeEncounterCard card) = card.id

instance HasField "code" SomeEncounterCard CardCode where
  getField (SomeEncounterCard card) = card.def.code

instance Show SomeCard where
  show (SomeCard card) = show card

instance ToJSON SomeCard where
  toJSON (SomeCard card) = toJSON card

data SomeCardDef where
  SomeCardDef
    :: (Show (CardDef k), Show (CardDetails k), ToJSON (CardDetails k)) => CardDef k -> SomeCardDef

instance HasField "code" SomeCardDef CardCode where
  getField (SomeCardDef def) = def.code

instance Show SomeCardDef where
  show (SomeCardDef def) = show def

instance (HasField "cost" (CardDetails k) ResourceCost) => HasField "cost" (CardDef k) ResourceCost where
  getField a = a.details.cost

type family EncounterCardDetails (k :: EncounterCardKind) where
  EncounterCardDetails 'QuestKind = QuestDetails
  EncounterCardDetails 'EnemyKind = EnemyDetails
  EncounterCardDetails 'TreacheryKind = TreacheryDetails
  EncounterCardDetails 'LocationKind = LocationDetails

data QuestDetails = QuestDetails
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EnemyDetails = EnemyDetails
  { engagementCost :: Int
  , threat :: Int
  , attack :: Int
  , defense :: Int
  , hitPoints :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TreacheryDetails = TreacheryDetails
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LocationDetails = LocationDetails {threat :: Int, questPoints :: Int}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type family CardDetails (k :: CardKind) where
  CardDetails 'HeroKind = HeroDetails
  CardDetails 'AllyKind = AllyDetails
  CardDetails 'AttachmentKind = AttachmentDetails
  CardDetails 'EventKind = EventDetails

data HeroDetails = HeroDetails
  { threat :: Int
  , willpower :: Int
  , attack :: Int
  , defense :: Int
  , hitPoints :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AllyDetails = AllyDetails
  { cost :: ResourceCost
  , willpower :: Int
  , attack :: Int
  , defense :: Int
  , hitPoints :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AttachmentDetails = AttachmentDetails
  { cost :: ResourceCost
  , restricted :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype EventDetails = EventDetails {cost :: ResourceCost}
  deriving newtype (Show, ToJSON)

data EncounterCardDef (k :: EncounterCardKind) = EncounterCardDef
  { code :: CardCode
  , name :: Name
  , unique :: Bool
  , kind :: SEncounterCardKind k
  , details :: EncounterCardDetails k
  , encounterSet :: EncounterSet
  , encounterSetCount :: (Int, Int, Int)
  }

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , name :: Name
  , unique :: Bool
  , kind :: SCardKind k
  , details :: CardDetails k
  , sphere :: Sphere
  }

deriving stock instance (Show (CardDetails k), Show (SCardKind k)) => Show (CardDef k)
deriving stock instance
  (Show (EncounterCardDetails k), Show (SEncounterCardKind k)) => Show (EncounterCardDef k)

instance (ToJSON (CardDetails k)) => ToJSON (CardDef k) where
  toJSON (CardDef {..}) =
    object
      [ "code" .= toJSON code
      , "name" .= toJSON name
      , "unique" .= toJSON unique
      , "kind" .= toJSON kind
      , "details" .= toJSON details
      , "sphere" .= toJSON sphere
      ]

instance (ToJSON (EncounterCardDetails k)) => ToJSON (EncounterCardDef k) where
  toJSON (EncounterCardDef {..}) =
    object
      [ "code" .= toJSON code
      , "name" .= toJSON name
      , "unique" .= toJSON unique
      , "kind" .= toJSON kind
      , "details" .= toJSON details
      ]

data Sphere = Leadership | Tactics | Spirit | Lore | Neutral
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Name = Name Text (Maybe Text)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsString Name where
  fromString s = Name (fromString s) Nothing

data ResourceCost = StaticCost Int | DynamicCost
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

cardCost :: SomeCard -> Maybe ResourceCost
cardCost (SomeCard card) = case card.def.kind of
  SHeroKind -> Nothing
  SAllyKind -> Just card.def.cost
  SAttachmentKind -> Just card.def.cost
  SEventKind -> Just card.def.cost

costAsResources :: ResourceCost -> Int
costAsResources = \case
  StaticCost n -> n
  DynamicCost -> 0

unique :: CardDef k -> CardDef k
unique def = def {unique = True}

class CanBeDynamic a where
  dynamic :: a -> a

instance CanBeDynamic (CardDef 'EventKind) where
  dynamic def = def {details = EventDetails DynamicCost}

deriving stock instance (Show (EncounterCardDetails k)) => Show (EncounterCard k)
deriving stock instance Generic (EncounterCard k)
deriving anyclass instance (ToJSON (EncounterCardDetails k)) => ToJSON (EncounterCard k)

newCard :: (MonadRandom m) => CardDef k -> Maybe PlayerId -> m (Card k)
newCard def pid = Card <$> getRandom <*> pure def <*> pure pid

genEncounterCard :: (MonadRandom m) => EncounterCardDef k -> m (EncounterCard k)
genEncounterCard def = EncounterCard <$> getRandom <*> pure def

data SomeEncounterCardDef where
  SomeEncounterCardDef
    :: (Show (EncounterCardDef k), Show (EncounterCardDetails k), ToJSON (EncounterCardDetails k))
    => EncounterCardDef k
    -> SomeEncounterCardDef

instance HasField "encounterSet" SomeEncounterCardDef EncounterSet where
  getField (SomeEncounterCardDef def) = def.encounterSet

instance HasField "encounterSetCount" SomeEncounterCardDef (Int, Int, Int) where
  getField (SomeEncounterCardDef def) = def.encounterSetCount

genSomeEncounterCard :: (MonadRandom m) => SomeEncounterCardDef -> m SomeEncounterCard
genSomeEncounterCard (SomeEncounterCardDef def) = SomeEncounterCard <$> (EncounterCard <$> getRandom <*> pure def)

instance FromJSON SomeEncounterCard where
  parseJSON = withObject "SomeEncounterCard" $ \v -> do
    kind <- v .: "kind"
    case kind of
      SomeSEncounterCardKind SQuestKind -> SomeEncounterCard <$> parseJSON @(EncounterCard 'QuestKind) (Object v)
      SomeSEncounterCardKind SEnemyKind -> SomeEncounterCard <$> parseJSON @(EncounterCard 'EnemyKind) (Object v)
      SomeSEncounterCardKind STreacheryKind -> SomeEncounterCard <$> parseJSON @(EncounterCard 'TreacheryKind) (Object v)
      SomeSEncounterCardKind SLocationKind -> SomeEncounterCard <$> parseJSON @(EncounterCard 'LocationKind) (Object v)

instance FromJSON (EncounterCard 'QuestKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind SQuestKind -> do
        id <- v .: "id"
        code <- v .: "code"
        name <- v .: "name"
        unique <- v .: "unique"
        details <- v .: "details"
        encounterSet <- v .: "encounterSet"
        encounterSetCount <- v .: "encounterSetCount"
        let kind = SQuestKind
        let def = EncounterCardDef {..}
        pure $ EncounterCard {..}
      _ -> fail "Expected LocationKind"

instance FromJSON (EncounterCard 'EnemyKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind SEnemyKind -> do
        id <- v .: "id"
        code <- v .: "code"
        name <- v .: "name"
        unique <- v .: "unique"
        details <- v .: "details"
        encounterSet <- v .: "encounterSet"
        encounterSetCount <- v .: "encounterSetCount"
        let kind = SEnemyKind
        let def = EncounterCardDef {..}
        pure $ EncounterCard {..}
      _ -> fail "Expected LocationKind"

instance FromJSON (EncounterCard 'TreacheryKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind STreacheryKind -> do
        id <- v .: "id"
        code <- v .: "code"
        name <- v .: "name"
        unique <- v .: "unique"
        details <- v .: "details"
        encounterSet <- v .: "encounterSet"
        encounterSetCount <- v .: "encounterSetCount"
        let kind = STreacheryKind
        let def = EncounterCardDef {..}
        pure $ EncounterCard {..}
      _ -> fail "Expected LocationKind"

instance FromJSON (EncounterCard 'LocationKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind SLocationKind -> do
        id <- v .: "id"
        code <- v .: "code"
        name <- v .: "name"
        unique <- v .: "unique"
        details <- v .: "details"
        encounterSet <- v .: "encounterSet"
        encounterSetCount <- v .: "encounterSetCount"
        let kind = SLocationKind
        let def = EncounterCardDef {..}
        pure $ EncounterCard {..}
      _ -> fail "Expected LocationKind"
