module LordOfTheRings.Sphere (module LordOfTheRings.Sphere) where

import LordOfTheRings.Prelude

-- | Fellowship and Baggins are the Saga Expansions' spheres: a Fellowship hero's resources
-- pay for Fellowship and neutral cards (The Black Riders rules insert), as Bilbo Baggins'
-- do for Baggins ones (The Hobbit: Over Hill and Under Hill rules insert).
data Sphere = Leadership | Tactics | Spirit | Lore | Neutral | Fellowship | Baggins
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
