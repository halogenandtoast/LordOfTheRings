module LordOfTheRings.Hero.Cards.Eleanor (eleanor) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Eleanor = Eleanor HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eleanor :: HeroCard Eleanor
eleanor = hero Eleanor Cards.eleanor
