module LordOfTheRings.Ability (module LordOfTheRings.Ability, module X) where

import LordOfTheRings.Ability.Types as X
import LordOfTheRings.Classes.HasAbilities as X
import LordOfTheRings.Criteria as X
import LordOfTheRings.Matcher.Window as X
import LordOfTheRings.Prelude
import LordOfTheRings.Source

mkAbility :: (Sourceable a) => a -> Int -> AbilityType -> Ability
mkAbility a idx aType = Ability (toSource a) idx aType NoCriteria NoLimit

restricted :: (Sourceable a) => a -> Int -> Criterion -> AbilityType -> Ability
restricted a idx criteria aType = Ability (toSource a) idx aType criteria NoLimit

limited :: AbilityLimit -> Ability -> Ability
limited l ab = ab {abilityLimit = l}

response :: WindowMatcher -> AbilityType
response w = ResponseAbility w Free

forced :: WindowMatcher -> AbilityType
forced = ForcedAbility

silent :: WindowMatcher -> AbilityType
silent = SilentForcedAbility
