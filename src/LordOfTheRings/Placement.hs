module LordOfTheRings.Placement (module LordOfTheRings.Placement) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Target

data Placement
  = InPlayArea PlayerId
  | AttachedTo Target
  | StagingArea
  | ActiveLocation
  | EngagedWith PlayerId
  | InVictoryDisplay
  | Unplaced
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

placementController :: Placement -> Maybe PlayerId
placementController = \case
  InPlayArea pid -> Just pid
  EngagedWith pid -> Just pid
  _ -> Nothing
