{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Card (module LordOfTheRings.Card) where

import Data.Aeson
import Data.String (IsString (..))
import GHC.Generics
import GHC.Records
import LordOfTheRings.Id
import LordOfTheRings.Prelude

data CardKind = HeroKind | AllyKind | AttachmentKind | EventKind

data SCardKind k where
  SHeroKind :: SCardKind 'HeroKind
  SAllyKind :: SCardKind 'AllyKind
  SAttachmentKind :: SCardKind 'AttachmentKind
  SEventKind :: SCardKind 'EventKind

deriving stock instance Show (SCardKind k)

instance ToJSON (SCardKind k) where
  toJSON = \case
    SHeroKind -> "hero"
    SAllyKind -> "ally"
    SAttachmentKind -> "attachment"
    SEventKind -> "event"

newtype CardCode = CardCode Text
  deriving newtype (Ord, Eq, IsString, Show, ToJSON, FromJSON)

data Card (k :: CardKind) = Card
  { id :: CardId
  , def :: CardDef k
  }

instance HasField "code" (Card k) CardCode where
  getField card = card.def.code

deriving stock instance (Show (CardDetails k)) => Show (Card k)

instance (ToJSON (CardDef k)) => ToJSON (Card k) where
  toJSON (Card cid def) = object ["id" .= toJSON cid, "def" .= toJSON def]

data SomeCard where
  SomeCard :: (Show (CardDef k), Show (CardDetails k), ToJSON (Card k)) => Card k -> SomeCard

instance HasField "code" SomeCard CardCode where
  getField (SomeCard card) = card.def.code

instance HasField "sphere" SomeCard Sphere where
  getField (SomeCard card) = card.def.sphere

instance HasField "cost" SomeCard (Maybe ResourceCost) where
  getField = cardCost

instance HasField "id" SomeCard CardId where
  getField (SomeCard card) = card.id

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
  deriving anyclass (ToJSON)

data AttachmentDetails = AttachmentDetails
  { cost :: ResourceCost
  , restricted :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype EventDetails = EventDetails {cost :: ResourceCost}
  deriving newtype (Show, ToJSON)

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , name :: Name
  , unique :: Bool
  , kind :: SCardKind k
  , details :: CardDetails k
  , sphere :: Sphere
  }

deriving stock instance (Show (CardDetails k), Show (SCardKind k)) => Show (CardDef k)

instance (ToJSON (CardDetails k)) => ToJSON (CardDef k) where
  toJSON (CardDef {..}) =
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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance IsString Name where
  fromString s = Name (fromString s) Nothing

data ResourceCost = StaticCost Int | DynamicCost
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

cardCost :: SomeCard -> Maybe ResourceCost
cardCost (SomeCard (Card _ def)) = case def.kind of
  SHeroKind -> Nothing
  SAllyKind -> Just def.cost
  SAttachmentKind -> Just def.cost
  SEventKind -> Just def.cost

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
