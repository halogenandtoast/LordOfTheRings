module LordOfTheRings.Hero.Cards.Theoden (theoden) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype Theoden = Theoden HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theoden :: HeroCard Theoden
theoden = hero Theoden Cards.theoden

-- | Each hero with a printed tactics resource icon gets +1 willpower.
instance HasModifiersFor Theoden where
  getModifiersFor (Theoden a) = do
    heroes <- select (HeroWithSphere Tactics)
    for_ heroes \h -> modified a h [WillpowerModifier 1]
