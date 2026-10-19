module LordOfTheRings.Hero.Cards.Aragorn (aragorn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Aragorn = Aragorn HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

aragorn :: HeroCard Aragorn
aragorn = hero Aragorn Cards.aragorn
