module LordOfTheRings.Message where

import Data.Aeson
import GHC.Generics

data Phase = ResourcePhase | PlanningPhase | QuestPhase | TravelPhase | EncounterPhase | CombatPhase
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Message = Setup | BeginPhase Phase | EndPhase Phase
  deriving stock (Show)
