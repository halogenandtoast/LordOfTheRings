module LordOfTheRings.Location.Cards.TrollshawForest (trollshawForest) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)

newtype TrollshawForest = TrollshawForest LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

trollshawForest :: LocationCard TrollshawForest
trollshawForest = location TrollshawForest Cards.trollshawForest

instance HasAbilities TrollshawForest where
  getAbilities (TrollshawForest a) = [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After | a.placement == StagingArea]

instance RunMessage TrollshawForest where
  runMessage msg l@(TrollshawForest a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      reduceRingBearersLife 1
      pure l
    TravelTo lid | lid == a.id -> do
      reduceRingBearersLife 1
      TrollshawForest <$> runMessage msg a
    _ -> TrollshawForest <$> runMessage msg a
