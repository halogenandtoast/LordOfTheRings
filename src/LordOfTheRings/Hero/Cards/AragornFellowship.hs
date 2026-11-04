module LordOfTheRings.Hero.Cards.AragornFellowship (aragornFellowship) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype AragornFellowship = AragornFellowship HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aragornFellowship :: HeroCard AragornFellowship
aragornFellowship = hero AragornFellowship Cards.aragornFellowship

instance RunMessage AragornFellowship where
  runMessage msg (AragornFellowship attrs) = AragornFellowship <$> runMessage msg attrs
