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
  | -- | an enemy making an attack from the shadows, as Wolf Rider's shadow effect: it attacks
    -- the player, but is neither engaged with him nor in the staging area
    AttackingFromShadow PlayerId
  | -- | in the Stormcaller's area (Flight of the Stormcaller): neither in the staging area
    -- nor engaged, until the players reach the Stormcaller's stage
    StormcallerArea
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

placementController :: Placement -> Maybe PlayerId
placementController = \case
  InPlayArea pid -> Just pid
  EngagedWith pid -> Just pid
  AttackingFromShadow pid -> Just pid
  _ -> Nothing
