module LordOfTheRings.Location.Cards.TowerGate (towerGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TowerGate = TowerGate LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

towerGate :: LocationCard TowerGate
towerGate = location TowerGate Cards.towerGate
