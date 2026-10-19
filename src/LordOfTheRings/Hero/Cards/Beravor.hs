module LordOfTheRings.Hero.Cards.Beravor (beravor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Beravor = Beravor HeroAttrs
  deriving anyclass (IsHero, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

beravor :: HeroCard Beravor
beravor = hero Beravor Cards.beravor
