module LordOfTheRings.Location.Cards.DenOfNight (denOfNight) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (placeResourceOnShelob, shelobTravelModifiers)

newtype DenOfNight = DenOfNight LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

denOfNight :: LocationCard DenOfNight
denOfNight = location DenOfNight Cards.denOfNight

-- | Travel: Place 1 resource on Shelob to travel here.
instance HasModifiersFor DenOfNight where
  getModifiersFor (DenOfNight a) = shelobTravelModifiers a

-- | While Den of Night is in the staging area, it gains: "Forced: After a 'When Revealed'
-- effect is canceled, place 1 resource on Shelob."
instance HasAbilities DenOfNight where
  getAbilities (DenOfNight a) = [mkAbility a 1 $ forced $ WhenRevealedCanceledWindow After | a.placement == StagingArea]

instance RunMessage DenOfNight where
  runMessage msg l@(DenOfNight a) = case msg of
    TravelTo lid | lid == a.id -> do
      placeResourceOnShelob a
      DenOfNight <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeResourceOnShelob a
      pure l
    _ -> DenOfNight <$> runMessage msg a
