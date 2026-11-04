module LordOfTheRings.Hero.Cards.BilboBagginsBaggins (bilboBagginsBaggins) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype BilboBagginsBaggins = BilboBagginsBaggins HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The first player gains control of Bilbo Baggins (his keyword).
bilboBagginsBaggins :: HeroCard BilboBagginsBaggins
bilboBagginsBaggins = hero BilboBagginsBaggins Cards.bilboBagginsBaggins

instance RunMessage BilboBagginsBaggins where
  runMessage msg (BilboBagginsBaggins attrs) = BilboBagginsBaggins <$> runMessage msg attrs
