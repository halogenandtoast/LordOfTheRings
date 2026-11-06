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
  | -- | both an objective and a neutral hero, as The Long Arm of Mordor's Haradrim: it has
    -- no threat cost and does not count toward the hero limit
    ObjectiveHeroType
  | -- | a scenario difficulty card kept beside its quest deck
    SetupType
  | CampaignType
  | -- | a Nightmare Deck's setup card, kept flipped beside the quest deck
    NightmareType
  | -- | a side quest from a player's deck, played into the staging area
    PlayerSideQuestType
  | -- | a side quest shuffled into the encounter deck, revealed into the staging area
    EncounterSideQuestType
  | -- | a contract, never in a deck: its player puts it into play with his heroes, side A
    -- faceup, and it may flip (MEC77 rules, "Contracts")
    ContractType
  | -- | an Ered Mithrin treasure: a player card discovered during the campaign, then added
    -- to a deck, 1 copy by title, not counting toward its 50 card minimum. It reads
    -- "Attach to", and is played and attached as an attachment is (MEC115 rules,
    -- "Treasure")
    TreasureType
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A side quest, from a player's deck or the encounter deck: one the players may quest
-- against in place of the main quest.
isSideQuestType :: CardType -> Bool
isSideQuestType = (`elem` [PlayerSideQuestType, EncounterSideQuestType])

isPlayerCardType :: CardType -> Bool
isPlayerCardType = (`elem` [HeroType, AllyType, AttachmentType, EventType, PlayerSideQuestType, ContractType, TreasureType])

-- | A card played onto a host and in play as an attachment: an attachment, or an Ered
-- Mithrin treasure, which reads "Attach to" as one does.
isAttachmentType :: CardType -> Bool
isAttachmentType = (`elem` [AttachmentType, TreasureType])

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
