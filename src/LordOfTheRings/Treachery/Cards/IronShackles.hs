module LordOfTheRings.Treachery.Cards.IronShackles (ironShackles) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IronShackles = IronShackles TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ironShackles :: TreacheryCard IronShackles
ironShackles = treachery IronShackles Cards.ironShackles
