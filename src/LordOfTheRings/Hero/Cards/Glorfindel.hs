module LordOfTheRings.Hero.Cards.Glorfindel (glorfindel) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Glorfindel = Glorfindel HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

glorfindel :: HeroCard Glorfindel
glorfindel = hero Glorfindel Cards.glorfindel
