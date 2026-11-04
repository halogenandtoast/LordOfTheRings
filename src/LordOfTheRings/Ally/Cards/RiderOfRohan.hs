module LordOfTheRings.Ally.Cards.RiderOfRohan (riderOfRohan) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype RiderOfRohan = RiderOfRohan AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

riderOfRohan :: AllyCard RiderOfRohan
riderOfRohan = ally RiderOfRohan Cards.riderOfRohan

-- | While a side quest is in the victory display, Rider of Rohan does not exhaust to quest.
instance HasModifiersFor RiderOfRohan where
  getModifiersFor (RiderOfRohan a) = do
    n <- sideQuestsInVictoryDisplay
    when (n > 0) $ modifySelf a [DoesNotExhaustToCommit]
