module LordOfTheRings.Event.Cards.UntroubledByDarkness (untroubledByDarkness) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Dark, Dwarf, Underground))

newtype UntroubledByDarkness = UntroubledByDarkness EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Each Dwarf character gets +1 willpower until the end of the phase. (+2
-- willpower instead if the active location is an Underground or Dark location.)
untroubledByDarkness :: EventCard UntroubledByDarkness
untroubledByDarkness = event UntroubledByDarkness Cards.untroubledByDarkness

instance RunMessage UntroubledByDarkness where
  runMessage msg e@(UntroubledByDarkness attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      deep <- or <$> traverse (selectAny . (IsActiveLocation <>) . LocationWithTrait) [Underground, Dark]
      dwarves <- selectCharacters (HeroWithTrait Dwarf) (AllyWithTrait Dwarf)
      for_ dwarves \c -> phaseModifier attrs c (WillpowerModifier $ if deep then 2 else 1)
      pure e
    _ -> UntroubledByDarkness <$> runMessage msg attrs
