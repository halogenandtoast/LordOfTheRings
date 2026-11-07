module LordOfTheRings.Ability.Types (module LordOfTheRings.Ability.Types) where

import LordOfTheRings.Criteria
import LordOfTheRings.Id (PlayerId, QuestId)
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Matcher.Types (AllyMatcher, HeroMatcher)
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
  | QuestProgressCost QuestId Int
  | QuestProgressPerPlayerCost QuestId Int
  | -- | the player advances The Uruk-hai pursuit tracker before resolving the effect
    IncreasePursuitCost Int
  | -- | the player spends resources of the sphere from their heroes' pools
    SphereResourceCost PlayerId Sphere Int
  | -- | the player exhausts a hero the matcher picks out, chosen as they pay, as Arkham's
    -- ExhaustAssetCost; 'ActivePlayer' in it is the player paying
    ExhaustHeroCost HeroMatcher
  | -- | the player deals this much damage to a hero the matcher picks out, chosen as they
    -- pay, as Gladden Marshlands' action; 'ActivePlayer' in it is the player paying
    DamageHeroCost HeroMatcher Int
  | Costs [AbilityCost]
  | -- | as 'ExhaustHeroCost', a hero or an ally the matchers pick out, as "Exhaust a Ranger
    -- character"
    ExhaustCharacterCost HeroMatcher AllyMatcher
  | -- | as 'SphereResourceCost', spent by whichever player uses the ability, as The Old
    -- Bridge's any player may
    SpendResourcesCost Sphere Int
  | GroupResourcesCost Sphere Int
  | -- | the paying player exhausts The One Ring, which he controls, as Master cards read
    ExhaustTheOneRingCost
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
data AbilityLimitType
  = PerPhase
  | -- | until the enemy's attack resolves, as "Limit once per attack"
    PerAttack
  | PerRound
  | -- | never reset, as "Limit once per game"
    PerGame
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
