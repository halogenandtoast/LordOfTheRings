module LordOfTheRings.Location.Cards.StockRoad (stockRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype StockRoad = StockRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stockRoad :: LocationCard StockRoad
stockRoad = location StockRoad Cards.stockRoad

-- | While Stock Road is in the staging area, add 2 to the total threat of the discarded
-- cards when a player resolves a Hide test.
instance HasModifiersFor StockRoad where
  getModifiersFor (StockRoad a) = when (a.placement == StagingArea) $ modified a GameTarget [HideTestThreat 2]

-- | Travel: The first player must make a Hide 1 test to travel here: made, the players
-- travel whether it passes or fails.
instance RunMessage StockRoad where
  runMessage msg (StockRoad a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      hideTest lead a 1
      StockRoad <$> runMessage msg a
    _ -> StockRoad <$> runMessage msg a
