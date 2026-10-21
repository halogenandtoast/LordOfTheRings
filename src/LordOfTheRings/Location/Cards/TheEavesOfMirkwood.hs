module LordOfTheRings.Location.Cards.TheEavesOfMirkwood (theEavesOfMirkwood) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TheEavesOfMirkwood = TheEavesOfMirkwood LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEavesOfMirkwood :: LocationCard TheEavesOfMirkwood
theEavesOfMirkwood = location TheEavesOfMirkwood Cards.theEavesOfMirkwood

-- | While The Eaves of Mirkwood is the active location, encounter card effects cannot be
-- canceled.
instance HasModifiersFor TheEavesOfMirkwood where
  getModifiersFor (TheEavesOfMirkwood a) = when (a.placement == ActiveLocation) $ modified a GameTarget [CannotCancelEncounterEffects]
