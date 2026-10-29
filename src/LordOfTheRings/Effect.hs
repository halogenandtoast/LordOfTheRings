module LordOfTheRings.Effect (module LordOfTheRings.Effect) where

import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

data EffectDuration
  = UntilEndOfPhase
  | UntilEndOfRound
  | -- | until the attack being made resolves
    UntilEndOfAttack
  | -- | until the card that made it spends it, as Minas Tirith Lampwright's "the next
    -- encounter card revealed"
    UntilSpent
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A lasting effect: modifiers on a target until the end of a phase or round.
data Effect = Effect
  { effectSource :: Source
  , effectTarget :: Target
  , effectModifiers :: [ModifierType]
  , effectDuration :: EffectDuration
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
