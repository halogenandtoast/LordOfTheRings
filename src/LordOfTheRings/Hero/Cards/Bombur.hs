module LordOfTheRings.Hero.Cards.Bombur (bombur) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Bombur = Bombur HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

bombur :: HeroCard Bombur
bombur = hero Bombur Cards.bombur

-- | When counting the number of Dwarf characters you control, Bombur counts as two (see
-- 'dwarfCount'). Exhausted for a cost, as We Are Not Idle's, he is still one Dwarf
-- (FAQ 1.9).
instance HasModifiersFor Bombur where
  getModifiersFor (Bombur a) = modifySelf a [CountsAsTwoDwarves]

instance RunMessage Bombur where
  runMessage msg (Bombur attrs) = Bombur <$> runMessage msg attrs
