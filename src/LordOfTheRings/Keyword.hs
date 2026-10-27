module LordOfTheRings.Keyword (module LordOfTheRings.Keyword) where

import LordOfTheRings.Prelude

data Keyword
  = Doomed Int
  | -- | an objective ally the first player gains control of as it enters play
    FirstPlayerGainsControl
  | Guarded
  | Ranged
  | -- | on an enemy: it heals this much damage as the refresh phase begins
    Regenerate Int
  | Restricted
  | Sentinel
  | Surge
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
