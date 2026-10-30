module LordOfTheRings.Hero.Cards.Ori (ori) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype Ori = Ori HeroAttrs
  deriving anyclass (IsHero, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ori :: HeroCard Ori
ori = hero Ori Cards.ori

{- | If you control at least 5 Dwarf characters, draw 1 additional card at the beginning
of the resource phase.

The additional card is part of the same draw, as Bilbo Baggins's, so it is not drawn when
the resource phase draws none.
-}
instance HasModifiersFor Ori where
  getModifiersFor (Ori a) = unless a.heroPrisoner do
    dwarves <- controlledCharactersWithTrait a.heroOwner Dwarf
    when (length dwarves >= 5) $ modified a (PlayerTarget a.heroOwner) [AdditionalResourcePhaseDraws 1]

instance RunMessage Ori where
  runMessage msg (Ori attrs) = Ori <$> runMessage msg attrs
