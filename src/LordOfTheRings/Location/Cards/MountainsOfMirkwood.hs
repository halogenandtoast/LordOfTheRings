module LordOfTheRings.Location.Cards.MountainsOfMirkwood (mountainsOfMirkwood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype MountainsOfMirkwood = MountainsOfMirkwood LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainsOfMirkwood :: LocationCard MountainsOfMirkwood
mountainsOfMirkwood = location MountainsOfMirkwood Cards.mountainsOfMirkwood
