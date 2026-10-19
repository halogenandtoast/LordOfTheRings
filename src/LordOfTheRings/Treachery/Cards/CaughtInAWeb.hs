module LordOfTheRings.Treachery.Cards.CaughtInAWeb (caughtInAWeb) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CaughtInAWeb = CaughtInAWeb TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

caughtInAWeb :: TreacheryCard CaughtInAWeb
caughtInAWeb = treachery CaughtInAWeb Cards.caughtInAWeb
