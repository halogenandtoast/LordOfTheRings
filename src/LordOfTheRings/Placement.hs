module LordOfTheRings.Placement where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Placement = Controlled PlayerId | Unplaced
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
