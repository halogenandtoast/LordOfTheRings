module LordOfTheRings.Quest.Type (module LordOfTheRings.Quest.Type) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class
  ( Typeable a
  , Show a
  , ToJSON a
  , RunMessage a
  , Entity a
  , EntityId a ~ QuestId
  , EntityAttrs a ~ QuestAttrs
  )
  => IsQuest a


data Quest = forall a. (IsQuest a) => Quest a

instance Entity Quest where
  type EntityId Quest = QuestId
  type EntityAttrs Quest = QuestAttrs
  toId = toId . toAttrs
  toAttrs (Quest a) = toAttrs a
  overAttrs f (Quest a) = Quest $ overAttrs f a

instance Entity QuestAttrs where
  type EntityId QuestAttrs = QuestId
  type EntityAttrs QuestAttrs = QuestAttrs
  toId = (.id)
  toAttrs a = a
  overAttrs f a = f a

data Side = A | B
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data QuestAttrs = QuestAttrs
  { id :: QuestId
  , card :: EncounterCard
  , side :: Side
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Quest where
  show (Quest a) = show a

instance ToJSON Quest where
  toJSON (Quest a) = toJSON a

instance Targetable QuestAttrs where
  toTarget a = QuestTarget a.id

instance Sourceable QuestAttrs where
  toSource a = QuestSource a.id
