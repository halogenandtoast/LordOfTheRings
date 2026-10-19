module LordOfTheRings.Location.Cards.GladdenFields (gladdenFields) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GladdenFields = GladdenFields LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gladdenFields :: LocationCard GladdenFields
gladdenFields = location GladdenFields Cards.gladdenFields
