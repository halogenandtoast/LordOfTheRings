module LordOfTheRings.Location.Cards.TheEastBight (theEastBight) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheEastBight = TheEastBight LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEastBight :: LocationCard TheEastBight
theEastBight = location TheEastBight Cards.theEastBight
