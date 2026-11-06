module LordOfTheRings.Effect (module LordOfTheRings.Effect) where

import LordOfTheRings.Id (EnemyId, CardId)
import LordOfTheRings.Modifier
import LordOfTheRings.Phase (Phase)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

data EffectDuration
  = UntilEndOfPhase
  | UntilEndOfRound
  | -- | until the attack being made resolves
    UntilEndOfAttack
  | -- | this enemy's attack; a nested player attack does not end it
    UntilEnemyAttackEnds EnemyId
  | -- | until the card that made it spends it, as Minas Tirith Lampwright's "the next
    -- encounter card revealed"
    UntilSpent
  | -- | until the end of the next such phase, this one if it is the phase now, as
    -- Enfeebled's "until the end of the combat phase"
    UntilEndOfPhaseOf Phase
  | -- | a shadow effect with no printed duration, while that card remains dealt
    UntilShadowCardLeaves EnemyId CardId
  | -- | until the engagement checks being made are over, as Fight Your Way Out's lowered
    -- engagement costs
    UntilEndOfEngagementChecks
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | A saved game from before 'UntilEndOfPhaseOf' holds a duration as its bare tag, as
-- aeson writes a type whose constructors all take nothing.
instance FromJSON EffectDuration where
  parseJSON = \case
    String tag -> genericParseJSON defaultOptions (object ["tag" .= tag])
    v -> genericParseJSON defaultOptions v

-- | A lasting effect: modifiers on a target until the end of a phase or round.
data Effect = Effect
  { effectSource :: Source
  , effectTarget :: Target
  , effectModifiers :: [ModifierType]
  , effectDuration :: EffectDuration
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
