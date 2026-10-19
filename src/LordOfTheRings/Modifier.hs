module LordOfTheRings.Modifier (module LordOfTheRings.Modifier) where

import LordOfTheRings.Prelude
import LordOfTheRings.Source

data ModifierType
  = WillpowerModifier Int
  | AttackModifier Int
  | DefenseModifier Int
  | HitPointModifier Int
  | ThreatModifier Int
  | EngagementCostModifier Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
