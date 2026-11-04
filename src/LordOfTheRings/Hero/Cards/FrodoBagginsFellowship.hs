module LordOfTheRings.Hero.Cards.FrodoBagginsFellowship (frodoBagginsFellowship) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FrodoBagginsFellowship = FrodoBagginsFellowship HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Black Riders' Ring-bearer, given to the first player as its scenarios are set up.
-- His Response is The Black Riders' player cards' to give.
frodoBagginsFellowship :: HeroCard FrodoBagginsFellowship
frodoBagginsFellowship = hero FrodoBagginsFellowship Cards.frodoBagginsFellowship

instance RunMessage FrodoBagginsFellowship where
  runMessage msg (FrodoBagginsFellowship a) = FrodoBagginsFellowship <$> runMessage msg a
