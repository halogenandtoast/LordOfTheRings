module LordOfTheRings.Message (module LordOfTheRings.Message) where

import {-# SOURCE #-} LordOfTheRings.Ability
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Window

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

data Drawing = Drawing
  { player :: PlayerId
  , count :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message
  = Setup
  | BeginPhase Phase
  | DoPhase Phase
  | EndPhase Phase
  | BeginRound
  | EndRound
  | WithTarget Target Message
  | PlayCard PlayerId CardId
  | PayCost CostId
  | PaidCost CostId
  | SpendResources PlayerId Sphere Int
  | SpendHeroResources HeroId Int
  | GainHeroResources HeroId Int
  | PlayAlly PlayerId (PlayerCard 'AllyKind)
  | DoStep Int Message
  | Ask PlayerId (Question Message)
  | Flip Target
  | Search PlayerId DeckSignifier CardMatcher CardHandler DeckHandler
  | AddToStaging SomeEncounterCard
  | ShuffleDeck DeckSignifier
  | RunWindow Window
  | UseCardAbility PlayerId Source Int
  | PlaceProgress Target Int
  | Run [Message]
  | DrawCards Drawing
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

choiceMessages :: PlayerId -> Choice Message -> [Message]
choiceMessages pid = \case
  Label _ msgs -> msgs
  TargetChoice _ msgs -> msgs
  UseAbilityLabel ability -> [UseCardAbility pid (abilitySource ability) (abilityIndex ability)]

chooseOrRunOne :: PlayerId -> [Choice Message] -> Message
chooseOrRunOne pid = \case
  [] -> error "chooseOrRunOne: empty list"
  [c] -> Run $ choiceMessages pid c
  cs -> Ask pid $ ChooseOne cs
