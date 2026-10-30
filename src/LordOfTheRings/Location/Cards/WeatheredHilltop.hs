module LordOfTheRings.Location.Cards.WeatheredHilltop (weatheredHilltop) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Weather))

newtype WeatheredHilltop = WeatheredHilltop LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

weatheredHilltop :: LocationCard WeatheredHilltop
weatheredHilltop = location WeatheredHilltop Cards.weatheredHilltop

-- | Weathered Hilltop gets +1 threat for each resource token on it.
instance HasModifiersFor WeatheredHilltop where
  getModifiersFor (WeatheredHilltop a) = do
    let n = countTokens Resource (locationTokens a)
    when (n > 0) $ modified a a [ThreatModifier n]

-- | Forced: After a Weather treachery is revealed from the encounter deck, place 1 resource
-- token here.
instance HasAbilities WeatheredHilltop where
  getAbilities (WeatheredHilltop a) = [mkAbility a 1 $ forced $ TreacheryWithTraitRevealedWindow After Weather]

instance RunMessage WeatheredHilltop where
  runMessage msg l@(WeatheredHilltop a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResources a a 1
      pure l
    _ -> WeatheredHilltop <$> runMessage msg a
