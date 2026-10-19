module LordOfTheRings.Location.Cards.EndlessCaverns (endlessCaverns) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype EndlessCaverns = EndlessCaverns LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

endlessCaverns :: LocationCard EndlessCaverns
endlessCaverns = location EndlessCaverns Cards.endlessCaverns
