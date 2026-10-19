module LordOfTheRings.Placement (module LordOfTheRings.Placement) where

import Control.Lens
import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Placement = Controlled PlayerId | Unplaced | Staging
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class HasPlacement a where
  placementL :: Lens' a Placement
