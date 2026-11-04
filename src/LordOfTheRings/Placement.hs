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
  | -- | in the Stormcaller's area (Flight of the Stormcaller): neither in the staging area
    -- nor engaged, until the players reach the Stormcaller's stage
    StormcallerArea
  | -- | in the Orcs' area (Race Across Harad): neither in the staging area nor engaged,
    -- while the players are not at the Orcs' stage
    OrcsArea
  | -- | in the riddle area (Dungeons Deep and Caverns Dim): neither in the staging area nor
    -- engaged, as Gollum waits there while Bilbo Baggins riddles with him
    RiddleArea
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What becomes of a player card an encounter card guards once its guard leaves play:
-- a Guarded (X) attachment attaches to an eligible card, a card Hobgoblin holds goes back
-- to its owner's hand (The Wilds of Rhovanion).
data GuardRelease = AttachWhenFree | ReturnToHandWhenFree
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

placementController :: Placement -> Maybe PlayerId
placementController = \case
  InPlayArea pid -> Just pid
  EngagedWith pid -> Just pid
  AttackingFromShadow pid -> Just pid
  _ -> Nothing
