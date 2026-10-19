module LordOfTheRings.Hero.Cards.Gimli (gimli) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Gimli = Gimli HeroAttrs
  deriving anyclass (IsHero, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gimli :: HeroCard Gimli
gimli = hero Gimli Cards.gimli
