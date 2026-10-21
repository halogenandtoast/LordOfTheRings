module LordOfTheRings.Hero.Cards.DainIronfoot (dainIronfoot) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DainIronfoot = DainIronfoot HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dainIronfoot :: HeroCard DainIronfoot
dainIronfoot = hero DainIronfoot Cards.dainIronfoot

-- | While Dáin Ironfoot is ready, Dwarf characters get +1 attack and +1 willpower.
instance HasModifiersFor DainIronfoot where
  getModifiersFor (DainIronfoot a) = unless a.heroExhausted do
    dwarves <- selectCharacters (HeroWithTrait Dwarf) (AllyWithTrait Dwarf)
    for_ dwarves \d -> modified a d [AttackModifier 1, WillpowerModifier 1]
