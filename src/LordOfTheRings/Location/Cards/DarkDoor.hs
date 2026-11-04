module LordOfTheRings.Location.Cards.DarkDoor (darkDoor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DarkDoor = DarkDoor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkDoor :: LocationCard DarkDoor
darkDoor = location DarkDoor Cards.darkDoor

-- | While Dark Door is in the staging area, progress cannot be placed on locations in the
-- staging area.
instance HasModifiersFor DarkDoor where
  getModifiersFor (DarkDoor a) = when (a.placement == StagingArea) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Travel: The first player reveals an encounter card to travel here.
instance RunMessage DarkDoor where
  runMessage msg (DarkDoor a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealEncounterCard
      DarkDoor <$> runMessage msg a
    _ -> DarkDoor <$> runMessage msg a
