module LordOfTheRings.Phase (Phase (..)) where

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
