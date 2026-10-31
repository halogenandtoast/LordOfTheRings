module LordOfTheRings.Location.Cards.OvergrownSteps (overgrownSteps) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OvergrownSteps = OvergrownSteps LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

overgrownSteps :: LocationCard OvergrownSteps
overgrownSteps = location OvergrownSteps Cards.overgrownSteps

-- | While Overgrown Steps is in the staging area, it gains: "Each Uncharted location in the
-- staging area gets +1 threat."
instance HasModifiersFor OvergrownSteps where
  getModifiersFor (OvergrownSteps a) = when (a.placement == StagingArea) do
    uncharted <- select (LocationInStagingArea <> unchartedLocation)
    for_ uncharted \l -> modified a l [ThreatModifier 1]

-- | Travel: Raise each player's threat by 2 to travel here.
instance RunMessage OvergrownSteps where
  runMessage msg l@(OvergrownSteps a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> encounterRaisesThreat p 2
      pure l
    _ -> OvergrownSteps <$> runMessage msg a
