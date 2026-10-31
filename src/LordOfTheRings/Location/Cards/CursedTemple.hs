module LordOfTheRings.Location.Cards.CursedTemple (cursedTemple, cursedTempleNightmare) where

import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under a Lost Island side.
newtype CursedTemple = CursedTemple LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedTemple :: LocationCard CursedTemple
cursedTemple = location CursedTemple Cards.cursedTemple

-- | The Fate of Númenor Nightmare Deck's reprint, the same text.
cursedTempleNightmare :: LocationCard CursedTemple
cursedTempleNightmare = location CursedTemple Cards.cursedTempleNightmare

-- | While Cursed Temple is the active location, characters with less than 2 willpower do
-- not ready during the refresh phase.
instance HasModifiersFor CursedTemple where
  getModifiersFor (CursedTemple a) = when (a.placement == ActiveLocation) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> do
      willpower <- getWillpower c
      when (willpower < 2) $ modified a c [DoesNotReadyDuringRefresh]

instance RunMessage CursedTemple where
  runMessage msg (CursedTemple a) = CursedTemple <$> runMessage msg a
