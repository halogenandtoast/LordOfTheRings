module LordOfTheRings.Ability (module LordOfTheRings.Ability) where

import LordOfTheRings.Id
import LordOfTheRings.Location.Type
import LordOfTheRings.Matcher
import LordOfTheRings.Prelude
import LordOfTheRings.Window

isForcedAbility :: Ability -> Bool
isForcedAbility ab = case ab.kind of
  Forced _ -> True
  Response _ -> False

windowMatches :: Window -> WindowMatcherTiming -> Bool
windowMatches (Window timing (EnterPlay target)) (WindowMatcherTiming timing' (EntersPlay target')) = target == target' && timing == timing'

data WindowMatcher = EntersPlay Target
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

entersPlay :: (Reference a) => a -> WindowMatcher
entersPlay = EntersPlay . toRef

data WindowMatcherTiming = WindowMatcherTiming Timing WindowMatcher
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Criteria = LocationExists (Matcher Location)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class Exists a where
  exists :: Matcher a -> Criteria

instance Exists Location where
  exists = LocationExists

toAbilityRef :: Ability -> AbilityRef
toAbilityRef a = AbilityRef (a.target) (a.index)

after :: WindowMatcher -> WindowMatcherTiming
after = WindowMatcherTiming After

data AbilityKind = Response WindowMatcherTiming | Forced WindowMatcherTiming
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Ability = Ability {kind :: AbilityKind, target :: Target, index :: Int, criteria :: Criteria, text :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

abilityWindow :: Ability -> Maybe WindowMatcherTiming
abilityWindow ab = case ab.kind of
  Response wmTiming -> Just wmTiming
  Forced wmTiming -> Just wmTiming

abilityTarget :: Ability -> Target
abilityTarget = (.target)

abilityIndex :: Ability -> Int
abilityIndex = (.index)

response :: (Reference a) => a -> Int -> WindowMatcherTiming -> Criteria -> Text -> Ability
response (toRef -> target) index wmTiming criteria text =
  Ability
    { target
    , index
    , criteria
    , text
    , kind = Response wmTiming
    }

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []
