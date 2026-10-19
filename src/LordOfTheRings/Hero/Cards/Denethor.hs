module LordOfTheRings.Hero.Cards.Denethor (denethor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Denethor = Denethor HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

denethor :: HeroCard Denethor
denethor = hero Denethor Cards.denethor
