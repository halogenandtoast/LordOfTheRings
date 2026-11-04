module LordOfTheRings.Location.Cards.ExposedRiverbank (exposedRiverbank) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheRiverRunning.Helpers (placeOnEasterlingPursuit)

newtype ExposedRiverbank = ExposedRiverbank LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

exposedRiverbank :: LocationCard ExposedRiverbank
exposedRiverbank = location ExposedRiverbank Cards.exposedRiverbank

-- | While Exposed Riverbank is in the staging area, it gains: "Forced: After a 'when
-- revealed' effect is canceled, place 1 resource on Easterling Pursuit."
instance HasAbilities ExposedRiverbank where
  getAbilities (ExposedRiverbank a) =
    [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After | a.placement == StagingArea]

-- | Travel: Place 1 resource on Easterling Pursuit.
instance RunMessage ExposedRiverbank where
  runMessage msg l@(ExposedRiverbank a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeOnEasterlingPursuit a 1
      pure l
    TravelTo lid | lid == a.id -> do
      placeOnEasterlingPursuit a 1
      pure l
    _ -> ExposedRiverbank <$> runMessage msg a
