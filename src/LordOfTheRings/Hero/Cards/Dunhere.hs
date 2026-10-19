module LordOfTheRings.Hero.Cards.Dunhere (dunhere) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Dunhere = Dunhere HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunhere :: HeroCard Dunhere
dunhere = hero Dunhere Cards.dunhere
