module LordOfTheRings.Message (module LordOfTheRings.Message) where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Phase
  = ResourcePhase
  | PlanningPhase
  | QuestPhase
  | TravelPhase
  | EncounterPhase
  | CombatPhase
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message
  = Setup
  | BeginPhase Phase
  | EndPhase Phase
  | BeginRound
  | EndRound
  | WithPlayer PlayerId Message
  | PlayCard PlayerId CardId
  | PayCost CostId
  | PaidCost CostId
  | SpendResources PlayerId Sphere Int
  | SpendHeroResources HeroId Int
  | PlayAlly PlayerId (Card 'AllyKind)
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
