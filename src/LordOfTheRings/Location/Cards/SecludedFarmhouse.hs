module LordOfTheRings.Location.Cards.SecludedFarmhouse (secludedFarmhouse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (withVillagers)

newtype SecludedFarmhouse = SecludedFarmhouse LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Villagers 2.
secludedFarmhouse :: LocationCard SecludedFarmhouse
secludedFarmhouse = location (SecludedFarmhouse . withVillagers 2) Cards.secludedFarmhouse

-- | Travel: Reveal the top card of the encounter deck and add it to the staging area to
-- travel here.
instance RunMessage SecludedFarmhouse where
  runMessage msg l@(SecludedFarmhouse a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      pure l
    _ -> SecludedFarmhouse <$> runMessage msg a
