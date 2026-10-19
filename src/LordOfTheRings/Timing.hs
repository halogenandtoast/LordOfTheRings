module LordOfTheRings.Timing (module LordOfTheRings.Timing) where

import LordOfTheRings.Prelude

data Timing = When | After
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
