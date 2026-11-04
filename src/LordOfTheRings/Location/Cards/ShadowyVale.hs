module LordOfTheRings.Location.Cards.ShadowyVale (shadowyVale) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ShadowyVale = ShadowyVale LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowyVale :: LocationCard ShadowyVale
shadowyVale = location ShadowyVale Cards.shadowyVale

-- | While any player has threat of 35 or higher, Shadowy Vale gets +3 threat.
instance HasModifiersFor ShadowyVale where
  getModifiersFor (ShadowyVale a) = whenM (selectAny $ PlayerWithThreatAtLeast 35) $ modifySelf a [ThreatModifier 3]

instance HasAbilities ShadowyVale where
  getAbilities (ShadowyVale a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

-- | Forced: After Shadowy Vale becomes the active location, reveal the top card of the
-- encounter deck.
instance RunMessage ShadowyVale where
  runMessage msg l@(ShadowyVale a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      revealEncounterCard
      pure l
    _ -> ShadowyVale <$> runMessage msg a
