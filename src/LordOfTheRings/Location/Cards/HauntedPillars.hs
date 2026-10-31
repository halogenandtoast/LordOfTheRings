module LordOfTheRings.Location.Cards.HauntedPillars (hauntedPillars) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype HauntedPillars = HauntedPillars LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntedPillars :: LocationCard HauntedPillars
hauntedPillars = location HauntedPillars Cards.hauntedPillars

-- | When faced with the option to travel, players must travel to Haunted Pillars, if able.
instance HasModifiersFor HauntedPillars where
  getModifiersFor (HauntedPillars a) = when (a.placement == StagingArea) $ modifySelf a [MustBeTraveledTo]

-- | When Revealed: Either return the active location to the staging area, or Haunted
-- Pillars gains surge.
instance RunMessage HauntedPillars where
  runMessage msg l@(HauntedPillars a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      active <- select IsActiveLocation
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "hauntedPillars.choose"
        causedBy a
        unless (null active) $ labeled "hauntedPillars.returnLocation" $ for_ active \x -> placeLocation x StagingArea
        labeled "hauntedPillars.surge" revealEncounterCard
      pure l
    _ -> HauntedPillars <$> runMessage msg a
