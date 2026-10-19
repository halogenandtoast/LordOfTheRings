module LordOfTheRings.Location.Cards.OldForestRoad (oldForestRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype OldForestRoad = OldForestRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

oldForestRoad :: LocationCard OldForestRoad
oldForestRoad = location OldForestRoad Cards.oldForestRoad
