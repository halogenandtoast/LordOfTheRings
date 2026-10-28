module LordOfTheRings.Location.Cards.BurningFarmhouse (burningFarmhouse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (discardVillagers, withVillagers)

newtype BurningFarmhouse = BurningFarmhouse LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Villagers 4.
burningFarmhouse :: LocationCard BurningFarmhouse
burningFarmhouse = location (BurningFarmhouse . withVillagers 4) Cards.burningFarmhouse

-- | Forced: At the end of the round, discard 1 villager token from Burning Farmhouse.
instance HasAbilities BurningFarmhouse where
  getAbilities (BurningFarmhouse a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage BurningFarmhouse where
  runMessage msg l@(BurningFarmhouse a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      void $ discardVillagers a a 1
      pure l
    _ -> BurningFarmhouse <$> runMessage msg a
