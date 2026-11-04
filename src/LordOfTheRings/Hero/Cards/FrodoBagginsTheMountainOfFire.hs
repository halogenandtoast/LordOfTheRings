module LordOfTheRings.Hero.Cards.FrodoBagginsTheMountainOfFire (frodoBagginsTheMountainOfFire) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FrodoBagginsTheMountainOfFire = FrodoBagginsTheMountainOfFire HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Mountain of Fire's Ring-bearer, as The Tower of Cirith Ungol sets him up. His
-- Planning Action is The Mountain of Fire player cards' to give him.
frodoBagginsTheMountainOfFire :: HeroCard FrodoBagginsTheMountainOfFire
frodoBagginsTheMountainOfFire = hero FrodoBagginsTheMountainOfFire Cards.frodoBagginsTheMountainOfFire

instance RunMessage FrodoBagginsTheMountainOfFire where
  runMessage msg (FrodoBagginsTheMountainOfFire attrs) = FrodoBagginsTheMountainOfFire <$> runMessage msg attrs
