module LordOfTheRings.Location.Cards.TheHeartOfTheMarshes (theHeartOfTheMarshes) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TheHeartOfTheMarshes = TheHeartOfTheMarshes LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theHeartOfTheMarshes :: LocationCard TheHeartOfTheMarshes
theHeartOfTheMarshes = location TheHeartOfTheMarshes Cards.theHeartOfTheMarshes

-- | While The Heart of the Marshes is the active location, all cards dealt from the
-- encounter deck for escape tests get +1 Escape.
instance HasModifiersFor TheHeartOfTheMarshes where
  getModifiersFor (TheHeartOfTheMarshes a) = when (a.placement == ActiveLocation) $ modified a GameTarget [EscapeBonus 1]
