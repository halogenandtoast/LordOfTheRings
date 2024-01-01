{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Card.EncounterCard (module LordOfTheRings.Card.EncounterCard) where

import Control.Monad.Random (MonadRandom (getRandom))
import Data.Aeson (Object, Value (..), object, parseJSON, withObject, withText, (.:), (.=))
import Data.Aeson.Types (Parser)
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Id
import LordOfTheRings.Name
import LordOfTheRings.Prelude hiding (id)

data EncounterCardKind = QuestKind | EnemyKind | TreacheryKind | LocationKind

data SEncounterCardKind k where
  SQuestKind :: SEncounterCardKind 'QuestKind
  SEnemyKind :: SEncounterCardKind 'EnemyKind
  STreacheryKind :: SEncounterCardKind 'TreacheryKind
  SLocationKind :: SEncounterCardKind 'LocationKind

data SomeSEncounterCardKind where
  SomeSEncounterCardKind :: SEncounterCardKind k -> SomeSEncounterCardKind

deriving stock instance Show (SEncounterCardKind k)

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

data EncounterCard (k :: EncounterCardKind) = MkEncounterCard
  { id :: CardId
  , def :: EncounterCardDef k
  }

data SomeEncounterCard where
  SomeEncounterCard
    :: (Show (EncounterCardDef k), Show (EncounterCardDetails k), ToJSON (EncounterCard k))
    => EncounterCard k
    -> SomeEncounterCard

instance Show SomeEncounterCard where
  show (SomeEncounterCard card) = show card

instance ToJSON SomeEncounterCard where
  toJSON (SomeEncounterCard card) = toJSON card

instance HasField "unique" SomeEncounterCard Bool where
  getField (SomeEncounterCard card) = card.def.unique

instance HasField "name" SomeEncounterCard Name where
  getField (SomeEncounterCard card) = card.def.name

instance HasField "id" SomeEncounterCard CardId where
  getField (SomeEncounterCard card) = card.id

instance HasField "code" SomeEncounterCard CardCode where
  getField (SomeEncounterCard card) = card.def.code

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

deriving stock instance
  (Show (EncounterCardDetails k), Show (SEncounterCardKind k)) => Show (EncounterCardDef k)

instance (ToJSON (EncounterCardDetails k)) => ToJSON (EncounterCardDef k) where
  toJSON (EncounterCardDef {..}) =
    object
      [ "code" .= toJSON code
      , "name" .= toJSON name
      , "unique" .= toJSON unique
      , "kind" .= toJSON kind
      , "details" .= toJSON details
      ]

deriving stock instance (Show (EncounterCardDetails k)) => Show (EncounterCard k)
deriving stock instance Generic (EncounterCard k)
deriving anyclass instance (ToJSON (EncounterCardDetails k)) => ToJSON (EncounterCard k)

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
      SomeSEncounterCardKind SQuestKind -> parseEncounterCard SQuestKind v
      _ -> fail "Expected AQuestKind"

instance FromJSON (EncounterCard 'EnemyKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind SEnemyKind -> parseEncounterCard SEnemyKind v
      _ -> fail "Expected EnemyKind"

instance FromJSON (EncounterCard 'TreacheryKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind STreacheryKind -> parseEncounterCard STreacheryKind v
      _ -> fail "Expected TreacheryKind"

instance FromJSON (EncounterCard 'LocationKind) where
  parseJSON = withObject "EncounterCard" $ \v -> do
    k <- v .: "kind"
    case k of
      SomeSEncounterCardKind SLocationKind -> parseEncounterCard SLocationKind v
      _ -> fail "Expected LocationKind"

parseEncounterCard
  :: forall k
   . (FromJSON (EncounterCardDetails k))
  => SEncounterCardKind k
  -> Object
  -> Parser (EncounterCard k)
parseEncounterCard kind v = do
  id <- v .: "id"
  code <- v .: "code"
  name <- v .: "name"
  unique <- v .: "unique"
  details <- v .: "details"
  encounterSet <- v .: "encounterSet"
  encounterSetCount <- v .: "encounterSetCount"
  let def = EncounterCardDef {..}
  pure $ MkEncounterCard {..}

data EncounterCardDef (k :: EncounterCardKind) = EncounterCardDef
  { code :: CardCode
  , name :: Name
  , unique :: Bool
  , kind :: SEncounterCardKind k
  , details :: EncounterCardDetails k
  , encounterSet :: EncounterSet
  , encounterSetCount :: (Int, Int, Int)
  }
