module LordOfTheRings.Hero.Cards.Ori (ori) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype Ori = Ori HeroAttrs
  deriving anyclass (IsHero, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ori :: HeroCard Ori
ori = hero Ori Cards.ori

-- | If you control at least 5 Dwarf characters, draw 1 additional card at the beginning
-- of the resource phase.
instance RunMessage Ori where
  runMessage msg h@(Ori attrs) = case msg of
    BeginPhase ResourcePhase -> do
      let you = PlayerWithId attrs.heroOwner
      dwarves <- selectCharacters (HeroControlledBy you <> HeroWithTrait Dwarf) (AllyControlledBy you <> AllyWithTrait Dwarf)
      when (length dwarves >= 5) $ drawCards attrs.heroOwner 1
      Ori <$> runMessage msg attrs
    _ -> Ori <$> runMessage msg attrs
