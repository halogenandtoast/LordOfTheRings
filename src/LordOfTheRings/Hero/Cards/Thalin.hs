module LordOfTheRings.Hero.Cards.Thalin (thalin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Thalin = Thalin HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

thalin :: HeroCard Thalin
thalin = hero Thalin Cards.thalin
