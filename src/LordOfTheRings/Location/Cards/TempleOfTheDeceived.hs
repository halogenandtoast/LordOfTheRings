module LordOfTheRings.Location.Cards.TempleOfTheDeceived (templeOfTheDeceived) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived.Helpers (gateKeyReady, getGateKey)

-- | The Temple side of the Island Map's rightmost column: Uncharted, over a Temple location.
newtype TempleOfTheDeceived = TempleOfTheDeceived LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

templeOfTheDeceived :: LocationCard TempleOfTheDeceived
templeOfTheDeceived = location TempleOfTheDeceived Cards.templeOfTheDeceived

-- | Immune to player card effects.
-- Travel: Exhaust the Gate Key to travel here.
instance HasModifiersFor TempleOfTheDeceived where
  getModifiersFor (TempleOfTheDeceived a) = do
    keyReady <- gateKeyReady
    modifySelf a $ ImmuneToPlayerCardEffects : [CannotBeTraveledTo | not keyReady]

-- | Forced: After Temple of the Deceived becomes the active location, flip it over.
instance HasAbilities TempleOfTheDeceived where
  getAbilities (TempleOfTheDeceived a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage TempleOfTheDeceived where
  runMessage msg l@(TempleOfTheDeceived a) = case msg of
    TravelTo lid | lid == a.id -> do
      traverse_ (exhaust . toTarget) =<< getGateKey
      pure l
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      flipOver a
      pure l
    _ -> TempleOfTheDeceived <$> runMessage msg a
