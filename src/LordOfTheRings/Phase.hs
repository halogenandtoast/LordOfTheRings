module LordOfTheRings.Phase where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Prelude

data Phase
  = ResourcePhase
  | PlanningPhase
  | QuestPhase
  | TravelPhase
  | EncounterPhase
  | CombatPhase
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
