module LordOfTheRings.Hero.Cards.Boromir (boromir) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype Boromir = Boromir HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

boromir :: HeroCard Boromir
boromir = hero Boromir Cards.boromir

-- | While Boromir has at least 1 resource in his resource pool, Gondor allies get +1
-- attack.
instance HasModifiersFor Boromir where
  getModifiersFor (Boromir a) = when (a.resources > 0) do
    allies <- select (AllyWithTrait Gondor)
    for_ allies \ally' -> modified a ally' [AttackModifier 1]
