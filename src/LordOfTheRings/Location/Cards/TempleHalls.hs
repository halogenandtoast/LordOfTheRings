module LordOfTheRings.Location.Cards.TempleHalls (templeHalls) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived.Helpers (templeGuardianReturns)

-- | Under a Temple of the Deceived side.
newtype TempleHalls = TempleHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

templeHalls :: LocationCard TempleHalls
templeHalls = location TempleHalls Cards.templeHalls

-- | While Temple Halls is the active location, each character gets -1 attack and -1 defense.
instance HasModifiersFor TempleHalls where
  getModifiersFor (TempleHalls a) = when (a.placement == ActiveLocation) do
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters \c -> modified a c [AttackModifier (-1), DefenseModifier (-1)]

-- | Forced: When Temple Halls is flipped over while active, put Temple Guardian into play
-- from the victory display, engaged with the player with the highest threat.
instance HasAbilities TempleHalls where
  getAbilities (TempleHalls a) = [mkAbility a 1 $ forced $ LocationFlippedWindow After (LocationWithId a.id <> IsActiveLocation)]

instance RunMessage TempleHalls where
  runMessage msg l@(TempleHalls a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      templeGuardianReturns a
      pure l
    _ -> TempleHalls <$> runMessage msg a
