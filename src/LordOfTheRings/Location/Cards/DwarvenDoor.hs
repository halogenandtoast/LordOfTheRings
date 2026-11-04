module LordOfTheRings.Location.Cards.DwarvenDoor (dwarvenDoor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype DwarvenDoor = DwarvenDoor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Deep.
dwarvenDoor :: LocationCard DwarvenDoor
dwarvenDoor = location DwarvenDoor Cards.dwarvenDoor

-- | Progress cannot be placed on Dwarven Door while it is in the staging area.
instance HasModifiersFor DwarvenDoor where
  getModifiersFor (DwarvenDoor a) = when (a.placement == StagingArea) $ modifySelf a [ProgressLimit 0]

instance RunMessage DwarvenDoor where
  runMessage msg (DwarvenDoor a) = DwarvenDoor <$> runMessage msg a
