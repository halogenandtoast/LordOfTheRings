module LordOfTheRings.Phase (Phase (..), nextPhase) where

import LordOfTheRings.Prelude

data Phase
  = ResourcePhase
  | PlanningPhase
  | QuestPhase
  | TravelPhase
  | EncounterPhase
  | CombatPhase
  | RefreshPhase
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

nextPhase :: Phase -> Maybe Phase
nextPhase phase
  | phase == maxBound = Nothing
  | otherwise = Just (succ phase)
