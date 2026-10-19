{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Card.PlayerCard (module LordOfTheRings.Card.PlayerCard) where

import LordOfTheRings.Card.Code
import LordOfTheRings.Id
import LordOfTheRings.Name (Name)
import LordOfTheRings.Prelude

data PlayerCard (k :: PlayerCardKind) = MkPlayerCard
  { id :: CardId
  , def :: PlayerCardDef k
  , owner :: Maybe PlayerId
  }

data PlayerCardKind = HeroKind | AllyKind | AttachmentKind | EventKind

data SPlayerCardKind k where
  SHeroKind :: SPlayerCardKind 'HeroKind
  SAllyKind :: SPlayerCardKind 'AllyKind
  SAttachmentKind :: SPlayerCardKind 'AttachmentKind
  SEventKind :: SPlayerCardKind 'EventKind

deriving stock instance Show (SPlayerCardKind k)

instance ToJSON (SPlayerCardKind k) where
  toJSON = \case
    SHeroKind -> "hero"
    SAllyKind -> "ally"
    SAttachmentKind -> "attachment"
    SEventKind -> "event"

instance FromJSON (SPlayerCardKind AllyKind) where
  parseJSON = withText "CardKind" $ \case
    "ally" -> pure SAllyKind
    _ -> fail "Invalid CardKind"

deriving stock instance Generic (PlayerCard 'AllyKind)
deriving stock instance Generic (PlayerCardDef 'AllyKind)
deriving anyclass instance FromJSON (PlayerCardDef 'AllyKind)
deriving anyclass instance FromJSON (PlayerCard 'AllyKind)

instance HasField "code" (PlayerCard k) CardCode where
  getField card = card.def.code

instance HasField "name" (PlayerCard k) Name where
  getField card = card.def.name

deriving stock instance (Show (PlayerCardDetails k)) => Show (PlayerCard k)

instance (ToJSON (PlayerCardDef k)) => ToJSON (PlayerCard k) where
  toJSON (MkPlayerCard cid def owner) = object ["id" .= toJSON cid, "def" .= toJSON def, "owner" .= toJSON owner]

data SomePlayerCard where
  SomePlayerCard
    :: (Show (PlayerCardDef k), Show (PlayerCardDetails k), ToJSON (PlayerCard k))
    => PlayerCard k
    -> SomePlayerCard

instance HasField "unique" SomePlayerCard Bool where
  getField (SomePlayerCard card) = card.def.unique

instance HasField "name" SomePlayerCard Name where
  getField (SomePlayerCard card) = card.def.name

instance HasField "code" SomePlayerCard CardCode where
  getField (SomePlayerCard card) = card.def.code

instance HasField "sphere" SomePlayerCard Sphere where
  getField (SomePlayerCard card) = card.def.sphere

instance HasField "cost" SomePlayerCard (Maybe ResourceCost) where
  getField = playerCardCost

instance HasField "id" SomePlayerCard CardId where
  getField (SomePlayerCard card) = card.id

instance Show SomePlayerCard where
  show (SomePlayerCard card) = show card

instance ToJSON SomePlayerCard where
  toJSON (SomePlayerCard card) = toJSON card

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

data Sphere = Leadership | Tactics | Spirit | Lore | Neutral
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ResourceCost = StaticCost Int | DynamicCost
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

class CanBeDynamic a where
  dynamic :: a -> a

instance CanBeDynamic (PlayerCardDef 'EventKind) where
  dynamic def = def {details = EventDetails DynamicCost}

newPlayerCard :: (MonadRandom m) => PlayerCardDef k -> Maybe PlayerId -> m (PlayerCard k)
newPlayerCard def pid = MkPlayerCard <$> getRandom <*> pure def <*> pure pid

playerCardCost :: SomePlayerCard -> Maybe ResourceCost
playerCardCost (SomePlayerCard card) = case card.def.kind of
  SHeroKind -> Nothing
  SAllyKind -> Just card.def.cost
  SAttachmentKind -> Just card.def.cost
  SEventKind -> Just card.def.cost
