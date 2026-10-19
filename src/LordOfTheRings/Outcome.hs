module LordOfTheRings.Outcome (module LordOfTheRings.Outcome) where

import LordOfTheRings.Prelude

data Outcome = Won | Lost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
