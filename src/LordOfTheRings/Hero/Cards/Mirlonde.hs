module LordOfTheRings.Hero.Cards.Mirlonde (mirlonde) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype Mirlonde = Mirlonde HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirlonde :: HeroCard Mirlonde
mirlonde = hero Mirlonde Cards.mirlonde

-- | Each hero you control with a printed lore resource icon gets -1 threat cost.
instance HasModifiersFor Mirlonde where
  getModifiersFor (Mirlonde a) = do
    heroes <- select (HeroControlledBy (PlayerWithId a.owner) <> HeroWithSphere Lore)
    for_ heroes \h -> modified a h [ThreatModifier (-1)]
