module LordOfTheRings.Hero.Cards.Eowyn (eowyn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Eowyn = Eowyn HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eowyn :: HeroCard Eowyn
eowyn = hero Eowyn Cards.eowyn
