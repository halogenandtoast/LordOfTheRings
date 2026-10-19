module LordOfTheRings.Ally.Cards.NorthernTracker (northernTracker) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype NorthernTracker = NorthernTracker AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

northernTracker :: AllyCard NorthernTracker
northernTracker = ally NorthernTracker Cards.northernTracker
