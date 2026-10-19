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

cardIs :: CardDef -> CardMatcher
cardIs = CardWithCode . cdCardCode

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
  | PlayAlly PlayerId PlayerCard
  | DoStep Int Message
  | Ask PlayerId (Question Message)
  | Flip Target
  | Search PlayerId DeckSignifier CardMatcher CardHandler DeckHandler
  | AddToStaging EncounterCard
  | ShuffleDeck DeckSignifier
  | RunWindow Window
  | UseCardAbility PlayerId Source Int
  | PlaceProgress Target Int
  | Run [Message]
  | DrawCards Drawing
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  AbilityLabel pid ability msgs -> Run $ UseCardAbility pid (abilitySource ability) (abilityIndex ability) : msgs

chooseOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error "chooseOne: no choices"
chooseOne pid choices = Ask pid $ ChooseOne choices

chooseOrRunOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid choices = chooseOne pid choices
