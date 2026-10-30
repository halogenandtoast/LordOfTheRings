module LordOfTheRings.Hero.Cards.ElfhelmLeadership (elfhelmLeadership) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership, Spirit, Tactics))
import LordOfTheRings.Trait (Trait (Mount))

newtype ElfhelmLeadership = ElfhelmLeadership HeroAttrs
  deriving anyclass (IsHero, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

elfhelmLeadership :: HeroCard ElfhelmLeadership
elfhelmLeadership = hero ElfhelmLeadership Cards.elfhelmLeadership

-- | Each spirit hero with an attached Mount gets +1 willpower.
-- Each tactics hero with an attached Mount gets +1 attack.
-- Each leadership hero with an attached Mount gets +1 defense.
instance HasModifiersFor ElfhelmLeadership where
  getModifiersFor (ElfhelmLeadership a) =
    for_ [(Spirit, WillpowerModifier 1), (Tactics, AttackModifier 1), (Leadership, DefenseModifier 1)] \(sphere, m) -> do
      mounted <- select (HeroWithAttached Mount <> HeroWithSphere sphere)
      for_ mounted \h -> modified a (HeroTarget h) [m]
