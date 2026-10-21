module LordOfTheRings.Ability (module LordOfTheRings.Ability, module X) where

import LordOfTheRings.Ability.Types as X
import LordOfTheRings.Classes.HasAbilities as X
import LordOfTheRings.Criteria as X
import LordOfTheRings.Matcher.Window as X
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Timing (Timing (After))

mkAbility :: (Sourceable a) => a -> Int -> AbilityType -> Ability
mkAbility a idx aType = Ability (toSource a) idx aType NoCriteria NoLimit False

restricted :: (Sourceable a) => a -> Int -> Criterion -> AbilityType -> Ability
restricted a idx criteria aType = Ability (toSource a) idx aType criteria NoLimit False

-- | Any player may trigger the ability, as "Any player may trigger this ability" reads;
-- its criteria read 'ActivePlayer' as the player it is offered to.
anyPlayer :: Ability -> Ability
anyPlayer ab = ab {abilityAnyPlayer = True}

limited :: AbilityLimit -> Ability -> Ability
limited l ab = ab {abilityLimit = l}

response :: WindowMatcher -> AbilityType
response w = ResponseAbility w Free

-- | The cost of exhausting the card.
exhaustCost :: (Targetable a) => a -> AbilityCost
exhaustCost = ExhaustCost . toTarget

-- | After the card enters play.
entersPlay :: (Targetable a) => a -> WindowMatcher
entersPlay = EntersPlayWindow After . toTarget

-- | After the character is dealt damage.
dealtDamage :: (Targetable a) => a -> WindowMatcher
dealtDamage = DealtDamageWindow After . toTarget

-- | After an attack the character defended has resolved.
defendedAttack :: (Targetable a) => a -> WindowMatcher
defendedAttack = DefendedAttackWindow After . Just . toTarget

-- | After an attack the character made has resolved.
attacked :: (Targetable a) => a -> WindowMatcher
attacked = AttackedWindow After . toTarget

forced :: WindowMatcher -> AbilityType
forced = ForcedAbility

silent :: WindowMatcher -> AbilityType
silent = SilentForcedAbility
