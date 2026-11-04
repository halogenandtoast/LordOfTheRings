module LordOfTheRings.Hero.Cards.FrodoBagginsTheRoadDarkens (frodoBagginsTheRoadDarkens) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype FrodoBagginsTheRoadDarkens = FrodoBagginsTheRoadDarkens HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsTheRoadDarkens :: HeroCard FrodoBagginsTheRoadDarkens
frodoBagginsTheRoadDarkens = hero FrodoBagginsTheRoadDarkens Cards.frodoBagginsTheRoadDarkens

instance RunMessage FrodoBagginsTheRoadDarkens where
  runMessage msg (FrodoBagginsTheRoadDarkens attrs) = FrodoBagginsTheRoadDarkens <$> runMessage msg attrs
