module LordOfTheRings.Location.Cards.StockWoods (stockWoods) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AShadowOfThePast.Helpers (failedHideTestThisRound)

newtype StockWoods = StockWoods LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stockWoods :: LocationCard StockWoods
stockWoods = location StockWoods Cards.stockWoods

instance HasModifiersFor StockWoods where
  getModifiersFor (StockWoods a) = when (a.placement == StagingArea) $ modified a GameTarget [HideCommitLimit 2]

instance RunMessage StockWoods where
  runMessage msg l@(StockWoods a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      hideTest lead a 1
      StockWoods <$> runMessage msg a
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      failed <- failedHideTestThisRound pid
      enemyGetsAttack (CardIdSource cid) eid (if failed then 3 else 2)
      pure l
    _ -> StockWoods <$> runMessage msg a
