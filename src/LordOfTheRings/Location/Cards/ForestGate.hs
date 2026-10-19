module LordOfTheRings.Location.Cards.ForestGate (forestGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ForestGate = ForestGate LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestGate :: LocationCard ForestGate
forestGate = location ForestGate Cards.forestGate
