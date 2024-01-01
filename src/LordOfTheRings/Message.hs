module LordOfTheRings.Message (module LordOfTheRings.Message) where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Question

data DeckSignifier = EncounterDeck
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CardMatcher = CardWithCode CardCode
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

cardIs :: EncounterCardDef k -> CardMatcher
cardIs = CardWithCode . (.code)

data CardHandler = ToStaging
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
data DeckHandler = Shuffle
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message
  = Setup
  | BeginPhase Phase
  | EndPhase Phase
  | BeginRound
  | EndRound
  | WithTarget Target Message
  | PlayCard PlayerId CardId
  | PayCost CostId
  | PaidCost CostId
  | SpendResources PlayerId Sphere Int
  | SpendHeroResources HeroId Int
  | PlayAlly PlayerId (PlayerCard 'AllyKind)
  | DoStep Int Message
  | Ask PlayerId (Question Message)
  | Flip Target
  | Search PlayerId DeckSignifier CardMatcher CardHandler DeckHandler
  | AddToStaging SomeEncounterCard
  | ShuffleDeck DeckSignifier
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
