module LordOfTheRings.Ability.Types (module LordOfTheRings.Ability.Types) where

import LordOfTheRings.Criteria
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Matcher.Window
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Target

data AbilityCost
  = Free
  | ExhaustCost Target
  | ResourceCost Target Int
  | DiscardFromHandCost PlayerId Int
  | RaiseThreatCost Int
  | -- | the player spends resources of the sphere from their heroes' pools
    SphereResourceCost PlayerId Sphere Int
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

-- | How often an ability may be used, as in Arkham: by each player, or by the group.
data AbilityLimitType = PerPhase | PerRound
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AbilityLimit
  = NoLimit
  | PlayerLimit AbilityLimitType Int
  | GroupLimit AbilityLimitType Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

abilityLimitType :: AbilityLimit -> Maybe AbilityLimitType
abilityLimitType = \case
  NoLimit -> Nothing
  PlayerLimit t _ -> Just t
  GroupLimit t _ -> Just t

data Ability = Ability
  { abilitySource :: Source
  , abilityIndex :: Int
  , abilityType :: AbilityType
  , abilityCriteria :: Criterion
  , abilityLimit :: AbilityLimit
  , -- | any player may trigger it, not only its controller
    abilityAnyPlayer :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Ability where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("abilityLimit", toJSON NoLimit), ("abilityAnyPlayer", toJSON False)]

-- | An ability used under a limit, kept until the limit's phase or round ends.
data UsedAbility = UsedAbility
  { usedAbilitySource :: Source
  , usedAbilityIndex :: Int
  , usedAbilityLimitType :: AbilityLimitType
  , usedTimes :: Int
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
