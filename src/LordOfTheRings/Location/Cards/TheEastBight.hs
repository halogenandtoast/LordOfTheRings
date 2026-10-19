module LordOfTheRings.Location.Cards.TheEastBight (theEastBight) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheEastBight = TheEastBight LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEastBight :: LocationCard TheEastBight
theEastBight = location TheEastBight Cards.theEastBight

-- | When faced with the option to travel, the players must travel to The East Bight if
-- there is no active location.
instance HasModifiersFor TheEastBight where
  getModifiersFor (TheEastBight a) = modified a a [MustBeTraveledTo]
