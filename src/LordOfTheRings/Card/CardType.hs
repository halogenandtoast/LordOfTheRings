module LordOfTheRings.Card.CardType (module LordOfTheRings.Card.CardType) where

import GHC.OverloadedLabels
import LordOfTheRings.Prelude

data CardType
  = HeroType
  | AllyType
  | AttachmentType
  | EventType
  | EnemyType
  | LocationType
  | TreacheryType
  | QuestType
  | ObjectiveType
  | ObjectiveAllyType
  | CampaignType
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

isPlayerCardType :: CardType -> Bool
isPlayerCardType = (`elem` [HeroType, AllyType, AttachmentType, EventType])

instance IsLabel "ally" CardType where
  fromLabel = AllyType

instance IsLabel "hero" CardType where
  fromLabel = HeroType

instance IsLabel "attachment" CardType where
  fromLabel = AttachmentType

instance IsLabel "event" CardType where
  fromLabel = EventType

instance IsLabel "enemy" CardType where
  fromLabel = EnemyType

instance IsLabel "location" CardType where
  fromLabel = LocationType

instance IsLabel "treachery" CardType where
  fromLabel = TreacheryType

instance IsLabel "quest" CardType where
  fromLabel = QuestType
