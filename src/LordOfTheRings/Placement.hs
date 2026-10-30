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
  | -- | a location on the Island Map neither active nor adjacent to the active location: in
    -- play, but not in the staging area (Temple of the Deceived)
    OnIslandMap
  | -- | an enemy making an attack from the shadows, as Wolf Rider's shadow effect: it attacks
    -- the player, but is neither engaged with him nor in the staging area
    AttackingFromShadow PlayerId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

placementController :: Placement -> Maybe PlayerId
placementController = \case
  InPlayArea pid -> Just pid
  EngagedWith pid -> Just pid
  AttackingFromShadow pid -> Just pid
  _ -> Nothing
