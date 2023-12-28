{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Card where

import Data.Aeson
import Data.String (IsString (..))
import Data.Text (Text)
import GHC.Generics
import GHC.Records
import LordOfTheRings.Id

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
  deriving newtype (Ord, Eq, IsString, Show, ToJSON)

data Card (k :: CardKind) = Card
  { id :: CardId
  , def :: CardDef k
  }

deriving stock instance (Show (CardDetails k)) => Show (Card k)

instance (ToJSON (CardDef k)) => ToJSON (Card k) where
  toJSON (Card cid def) = object ["id" .= toJSON cid, "def" .= toJSON def]

data SomeCard where
  SomeCard :: (Show (CardDef k), Show (CardDetails k), ToJSON (Card k)) => Card k -> SomeCard

instance HasField "code" SomeCard CardCode where
  getField (SomeCard card) = card.def.code

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

instance (HasField "cost" (CardDetails k) Cost) => HasField "cost" (CardDef k) Cost where
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
  { cost :: Cost
  , willpower :: Int
  , attack :: Int
  , defense :: Int
  , hitPoints :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data AttachmentDetails = AttachmentDetails
  { cost :: Cost
  , restricted :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype EventDetails = EventDetails {cost :: Cost}
  deriving newtype (Show, ToJSON)

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , name :: Name
  , unique :: Bool
  , kind :: SCardKind k
  , details :: CardDetails k
  , spheres :: [Sphere]
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
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Name = Name Text (Maybe Text)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance IsString Name where
  fromString s = Name (fromString s) Nothing

data Cost = StaticCost Int | DynamicCost
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
