module LordOfTheRings.Ability (module LordOfTheRings.Ability) where

import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Matcher.Types
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

entersPlay :: (Targetable a) => a -> WindowMatcher
entersPlay = EntersPlay . toTarget

data WindowMatcherTiming = WindowMatcherTiming Timing WindowMatcher
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Criteria = LocationExists LocationMatcher
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class Exists a where
  exists :: a -> Criteria

instance Exists LocationMatcher where
  exists = LocationExists

after :: WindowMatcher -> WindowMatcherTiming
after = WindowMatcherTiming After

data AbilityKind = Response WindowMatcherTiming | Forced WindowMatcherTiming
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Ability = Ability {kind :: AbilityKind, source :: Source, index :: Int, criteria :: Criteria, text :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

abilityWindow :: Ability -> Maybe WindowMatcherTiming
abilityWindow ab = case ab.kind of
  Response wmTiming -> Just wmTiming
  Forced wmTiming -> Just wmTiming

abilitySource :: Ability -> Source
abilitySource = (.source)

abilityIndex :: Ability -> Int
abilityIndex = (.index)

response :: (Sourceable a) => a -> Int -> WindowMatcherTiming -> Criteria -> Text -> Ability
response (toSource -> source) index wmTiming criteria text =
  Ability
    { source
    , index
    , criteria
    , text
    , kind = Response wmTiming
    }

class HasAbilities a where
  getAbilities :: a -> [Ability]
  getAbilities _ = []
