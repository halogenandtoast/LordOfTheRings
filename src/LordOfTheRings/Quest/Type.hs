module LordOfTheRings.Quest.Type (module LordOfTheRings.Quest.Type) where

import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class (Show a, ToJSON a, RunMessage a) => IsQuest a

data Quest where
  Quest :: (IsQuest a) => a -> Quest

data Side = A | B
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data QuestAttrs = QuestAttrs
  { id :: QuestId
  , card :: EncounterCard 'QuestKind
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
