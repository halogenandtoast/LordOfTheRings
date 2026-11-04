module LordOfTheRings.Location.Cards.BurntSands (burntSands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (getTemperature, increaseTemperature)

newtype BurntSands = BurntSands LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

burntSands :: LocationCard BurntSands
burntSands = location BurntSands Cards.burntSands

-- | While the temperature is 40 or higher, Burnt Sands gets +2 threat.
instance HasModifiersFor BurntSands where
  getModifiersFor (BurntSands a) = do
    t <- getTemperature
    when (t >= 40) $ modifySelf a [ThreatModifier 2]

-- | Forced: After Burnt Sands becomes the active location, increase the temperature by 2.
instance HasAbilities BurntSands where
  getAbilities (BurntSands a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage BurntSands where
  runMessage msg l@(BurntSands a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      increaseTemperature 2
      pure l
    _ -> BurntSands <$> runMessage msg a
