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

data PlayerCardKind = HeroKind | AllyKind | AttachmentKind | EventKind
data EncounterCardKind = QuestKind | EnemyKind | TreacheryKind | LocationKind

data SPlayerCardKind k where
  SHeroKind :: SPlayerCardKind 'HeroKind
  SAllyKind :: SPlayerCardKind 'AllyKind
  SAttachmentKind :: SPlayerCardKind 'AttachmentKind
  SEventKind :: SPlayerCardKind 'EventKind

data SEncounterCardKind k where
  SQuestKind :: SEncounterCardKind 'QuestKind
  SEnemyKind :: SEncounterCardKind 'EnemyKind
  STreacheryKind :: SEncounterCardKind 'TreacheryKind
  SLocationKind :: SEncounterCardKind 'LocationKind

data SomeSEncounterCardKind where
  SomeSEncounterCardKind :: SEncounterCardKind k -> SomeSEncounterCardKind

deriving stock instance Show (SPlayerCardKind k)
deriving stock instance Show (SEncounterCardKind k)

instance ToJSON (SPlayerCardKind k) where
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

data PlayerCard (k :: PlayerCardKind) = MkPlayerCard
  { id :: CardId
  , def :: PlayerCardDef k
  , owner :: Maybe PlayerId
  }

data EncounterCard (k :: EncounterCardKind) = MkEncounterCard
  { id :: CardId
  , def :: EncounterCardDef k
  }

data Card = PlayerCard SomePlayerCard | EncounterCard SomeEncounterCard
  deriving stock (Show, Generic)

instance ToJSON Card where
  toJSON = \case
    PlayerCard pc -> toJSON pc
    EncounterCard ec -> toJSON ec

instance FromJSON (SPlayerCardKind AllyKind) where
  parseJSON = withText "CardKind" $ \case
    "ally" -> pure SAllyKind
    _ -> fail "Invalid CardKind"

deriving stock instance Generic (PlayerCard 'AllyKind)
deriving stock instance Generic (PlayerCardDef 'AllyKind)
deriving anyclass instance FromJSON (PlayerCardDef 'AllyKind)
deriving anyclass instance FromJSON (PlayerCard 'AllyKind)

instance HasField "id" Card CardId where
  getField = \case
    PlayerCard pc -> pc.id
    EncounterCard ec -> ec.id

instance HasField "name" Card Name where
  getField = \case
    PlayerCard pc -> pc.name
    EncounterCard ec -> ec.name

instance HasField "unique" Card Bool where
  getField = \case
    PlayerCard pc -> pc.unique
    EncounterCard ec -> ec.unique

instance HasField "sphere" Card (Maybe Sphere) where
  getField = \case
    PlayerCard pc -> Just pc.sphere
    EncounterCard _ -> Nothing

instance HasField "cost" Card (Maybe ResourceCost) where
  getField = cardCost

instance HasField "code" (PlayerCard k) CardCode where
  getField card = card.def.code

instance HasField "name" (PlayerCard k) Name where
  getField card = card.def.name

deriving stock instance (Show (PlayerCardDetails k)) => Show (PlayerCard k)

instance (ToJSON (PlayerCardDef k)) => ToJSON (PlayerCard k) where
  toJSON (MkPlayerCard cid def owner) = object ["id" .= toJSON cid, "def" .= toJSON def, "owner" .= toJSON owner]

data SomeEncounterCard where
  SomeEncounterCard
    :: (Show (EncounterCardDef k), Show (EncounterCardDetails k), ToJSON (EncounterCard k))
    => EncounterCard k
    -> SomeEncounterCard

instance Show SomeEncounterCard where
  show (SomeEncounterCard card) = show card

instance ToJSON SomeEncounterCard where
  toJSON (SomeEncounterCard card) = toJSON card

data SomePlayerCard where
  SomePlayerCard
    :: (Show (PlayerCardDef k), Show (PlayerCardDetails k), ToJSON (PlayerCard k))
    => PlayerCard k
    -> SomePlayerCard

instance HasField "unique" SomePlayerCard Bool where
  getField (SomePlayerCard card) = card.def.unique

instance HasField "unique" SomeEncounterCard Bool where
  getField (SomeEncounterCard card) = card.def.unique

instance HasField "name" SomePlayerCard Name where
  getField (SomePlayerCard card) = card.def.name

instance HasField "name" SomeEncounterCard Name where
  getField (SomeEncounterCard card) = card.def.name

instance HasField "code" SomePlayerCard CardCode where
  getField (SomePlayerCard card) = card.def.code

instance HasField "sphere" SomePlayerCard Sphere where
  getField (SomePlayerCard card) = card.def.sphere

instance HasField "cost" SomePlayerCard (Maybe ResourceCost) where
  getField = cardCost . PlayerCard

instance HasField "id" SomePlayerCard CardId where
  getField (SomePlayerCard card) = card.id

instance HasField "id" SomeEncounterCard CardId where
  getField (SomeEncounterCard card) = card.id

instance HasField "code" SomeEncounterCard CardCode where
  getField (SomeEncounterCard card) = card.def.code

instance Show SomePlayerCard where
  show (SomePlayerCard card) = show card

instance ToJSON SomePlayerCard where
  toJSON (SomePlayerCard card) = toJSON card

data SomeCardDef = SomePlayerCardDef SomePlayerCardDef | SomeEncounterCardDef SomeEncounterCardDef

data SomePlayerCardDef where
  MkSomePlayerCardDef
    :: (Show (PlayerCardDef k), Show (PlayerCardDetails k), ToJSON (PlayerCardDetails k))
    => PlayerCardDef k
    -> SomePlayerCardDef

instance HasField "code" SomePlayerCardDef CardCode where
  getField (MkSomePlayerCardDef def) = def.code

instance Show SomePlayerCardDef where
  show (MkSomePlayerCardDef def) = show def

instance
  (HasField "cost" (PlayerCardDetails k) ResourceCost)
  => HasField "cost" (PlayerCardDef k) ResourceCost
  where
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

type family PlayerCardDetails (k :: PlayerCardKind) where
  PlayerCardDetails 'HeroKind = HeroDetails
  PlayerCardDetails 'AllyKind = AllyDetails
  PlayerCardDetails 'AttachmentKind = AttachmentDetails
  PlayerCardDetails 'EventKind = EventDetails

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

data PlayerCardDef (k :: PlayerCardKind) = PlayerCardDef
  { code :: CardCode
  , name :: Name
  , unique :: Bool
  , kind :: SPlayerCardKind k
  , details :: PlayerCardDetails k
  , sphere :: Sphere
  }

deriving stock instance
  (Show (PlayerCardDetails k), Show (SPlayerCardKind k)) => Show (PlayerCardDef k)
deriving stock instance
  (Show (EncounterCardDetails k), Show (SEncounterCardKind k)) => Show (EncounterCardDef k)

instance (ToJSON (PlayerCardDetails k)) => ToJSON (PlayerCardDef k) where
  toJSON (PlayerCardDef {..}) =
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

cardCost :: Card -> Maybe ResourceCost
cardCost (PlayerCard (SomePlayerCard card)) = case card.def.kind of
  SHeroKind -> Nothing
  SAllyKind -> Just card.def.cost
  SAttachmentKind -> Just card.def.cost
  SEventKind -> Just card.def.cost

costAsResources :: ResourceCost -> Int
costAsResources = \case
  StaticCost n -> n
  DynamicCost -> 0

unique :: PlayerCardDef k -> PlayerCardDef k
unique def = def {unique = True}

class CanBeDynamic a where
  dynamic :: a -> a

instance CanBeDynamic (PlayerCardDef 'EventKind) where
  dynamic def = def {details = EventDetails DynamicCost}

deriving stock instance (Show (EncounterCardDetails k)) => Show (EncounterCard k)
deriving stock instance Generic (EncounterCard k)
deriving anyclass instance (ToJSON (EncounterCardDetails k)) => ToJSON (EncounterCard k)

newPlayerCard :: (MonadRandom m) => PlayerCardDef k -> Maybe PlayerId -> m (PlayerCard k)
newPlayerCard def pid = MkPlayerCard <$> getRandom <*> pure def <*> pure pid

newEncounterCard :: (MonadRandom m) => EncounterCardDef k -> m (EncounterCard k)
newEncounterCard def = MkEncounterCard <$> getRandom <*> pure def

genEncounterCard :: (MonadRandom m) => EncounterCardDef k -> m (EncounterCard k)
genEncounterCard def = MkEncounterCard <$> getRandom <*> pure def

data SomeEncounterCardDef where
  MkSomeEncounterCardDef
    :: (Show (EncounterCardDef k), Show (EncounterCardDetails k), ToJSON (EncounterCardDetails k))
    => EncounterCardDef k
    -> SomeEncounterCardDef

instance HasField "encounterSet" SomeEncounterCardDef EncounterSet where
  getField (MkSomeEncounterCardDef def) = def.encounterSet

instance HasField "encounterSetCount" SomeEncounterCardDef (Int, Int, Int) where
  getField (MkSomeEncounterCardDef def) = def.encounterSetCount

genSomeEncounterCard :: (MonadRandom m) => SomeEncounterCardDef -> m SomeEncounterCard
genSomeEncounterCard (MkSomeEncounterCardDef def) = SomeEncounterCard <$> (MkEncounterCard <$> getRandom <*> pure def)

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
        pure $ MkEncounterCard {..}
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
        pure $ MkEncounterCard {..}
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
        pure $ MkEncounterCard {..}
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
        pure $ MkEncounterCard {..}
      _ -> fail "Expected LocationKind"
