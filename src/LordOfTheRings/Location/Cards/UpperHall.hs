module LordOfTheRings.Location.Cards.UpperHall (upperHall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype UpperHall = UpperHall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Doomed 2.
upperHall :: LocationCard UpperHall
upperHall = location UpperHall Cards.upperHall
