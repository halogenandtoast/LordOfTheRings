module LordOfTheRings.Hero.Cards.Legolas (legolas) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Legolas = Legolas HeroAttrs
  deriving anyclass (IsHero, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

legolas :: HeroCard Legolas
legolas = hero Legolas Cards.legolas
