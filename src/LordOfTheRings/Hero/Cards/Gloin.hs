module LordOfTheRings.Hero.Cards.Gloin (gloin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Gloin = Gloin HeroAttrs
  deriving anyclass (IsHero, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gloin :: HeroCard Gloin
gloin = hero Gloin Cards.gloin
