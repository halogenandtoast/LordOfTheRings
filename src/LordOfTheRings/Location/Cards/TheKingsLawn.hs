module LordOfTheRings.Location.Cards.TheKingsLawn (theKingsLawn) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))

newtype TheKingsLawn = TheKingsLawn LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingsLawn :: LocationCard TheKingsLawn
theKingsLawn = location TheKingsLawn Cards.theKingsLawn

-- | While The King's Lawn is in the staging area, each player must raise his threat by 1
-- additional point during the refresh phase: each copy there raises it once more.
instance HasAbilities TheKingsLawn where
  getAbilities (TheKingsLawn a) =
    [restricted a 1 (LocationExists $ LocationWithId a.id <> LocationInStagingArea) $ forced $ PhaseBeginsWindow After RefreshPhase]

-- | Travel: Place 1 resource on the main quest.
instance RunMessage TheKingsLawn where
  runMessage msg l@(TheKingsLawn a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> encounterRaisesThreat p 1
      pure l
    TravelTo lid | lid == a.id -> do
      placeResourcesOnMainQuest a 1
      TheKingsLawn <$> runMessage msg a
    _ -> TheKingsLawn <$> runMessage msg a
