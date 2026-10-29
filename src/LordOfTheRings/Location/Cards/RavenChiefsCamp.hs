module LordOfTheRings.Location.Cards.RavenChiefsCamp (ravenChiefsCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RavenChiefsCamp = RavenChiefsCamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenChiefsCamp :: LocationCard RavenChiefsCamp
ravenChiefsCamp = location RavenChiefsCamp Cards.ravenChiefsCamp

-- | Immune to player card effects.
instance HasModifiersFor RavenChiefsCamp where
  getModifiersFor (RavenChiefsCamp a) = modifySelf a [ImmuneToPlayerCardEffects]

-- | Forced: After the last time counter is removed from this location, exhaust each
-- damaged character. Place 3 time counters on this location.
instance HasAbilities RavenChiefsCamp where
  getAbilities (RavenChiefsCamp a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a]

instance RunMessage RavenChiefsCamp where
  runMessage msg l@(RavenChiefsCamp a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      characters <- selectCharacters AnyHero (AllyControlledBy AnyPlayer)
      damaged <- filterM (fmap (> 0) . getDamage) characters
      traverse_ exhaust damaged
      placeTimeCounters a a 3
      pure l
    _ -> RavenChiefsCamp <$> runMessage msg a
