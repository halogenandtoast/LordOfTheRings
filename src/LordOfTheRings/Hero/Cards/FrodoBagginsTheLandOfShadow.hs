module LordOfTheRings.Hero.Cards.FrodoBagginsTheLandOfShadow (frodoBagginsTheLandOfShadow) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

-- | The Land of Shadow's Ring-bearer, the hero its scenarios' setup hands the first player.
newtype FrodoBagginsTheLandOfShadow = FrodoBagginsTheLandOfShadow HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsTheLandOfShadow :: HeroCard FrodoBagginsTheLandOfShadow
frodoBagginsTheLandOfShadow = hero FrodoBagginsTheLandOfShadow Cards.frodoBagginsTheLandOfShadow
