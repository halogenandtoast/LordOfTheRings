module LordOfTheRings.Location.Cards.TheBrownLands (theBrownLands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheBrownLands = TheBrownLands LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBrownLands :: LocationCard TheBrownLands
theBrownLands = location TheBrownLands Cards.theBrownLands
