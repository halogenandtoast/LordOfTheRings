module LordOfTheRings.Hero.Cards.Gimli (gimli) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Gimli = Gimli HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gimli :: HeroCard Gimli
gimli = hero Gimli Cards.gimli

-- | Gimli gets +1 attack for each damage token on him.
instance HasModifiersFor Gimli where
  getModifiersFor (Gimli a) = when (a.damage > 0) $ modified a a [AttackModifier a.damage]
