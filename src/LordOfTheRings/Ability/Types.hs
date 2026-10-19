module LordOfTheRings.Ability.Types (module LordOfTheRings.Ability.Types) where

import LordOfTheRings.Criteria
import LordOfTheRings.Matcher.Window
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

data AbilityCost
  = Free
  | ExhaustCost Target
  | ResourceCost Target Int
  | Costs [AbilityCost]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AbilityType
  = ActionAbility AbilityCost
  | ResponseAbility WindowMatcher AbilityCost
  | ForcedAbility WindowMatcher
  | SilentForcedAbility WindowMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityCriteria :: Criterion
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

abilityWindow :: Ability -> Maybe WindowMatcher
abilityWindow ab = case abilityType ab of
  ActionAbility _ -> Nothing
  ResponseAbility w _ -> Just w
  ForcedAbility w -> Just w
  SilentForcedAbility w -> Just w

abilityCost :: Ability -> AbilityCost
abilityCost ab = case abilityType ab of
  ActionAbility c -> c
  ResponseAbility _ c -> c
  ForcedAbility _ -> Free
  SilentForcedAbility _ -> Free

isForcedAbility :: Ability -> Bool
isForcedAbility ab = case abilityType ab of
  ForcedAbility _ -> True
  SilentForcedAbility _ -> True
  _ -> False

isResponseAbility :: Ability -> Bool
isResponseAbility ab = case abilityType ab of
  ResponseAbility {} -> True
  _ -> False
