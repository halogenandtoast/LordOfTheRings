module LordOfTheRings.Quest.Type (module LordOfTheRings.Quest.Type) where

import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Show a, ToJSON a, Run a) => IsQuest a

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

instance Reference QuestAttrs where
  toRef a = QuestRef a.id
