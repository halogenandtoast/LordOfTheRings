module LordOfTheRings.Card.CardType (module LordOfTheRings.Card.CardType) where

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
