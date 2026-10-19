module LordOfTheRings.Treachery.Cards.EvilStorm (evilStorm) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EvilStorm = EvilStorm TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

evilStorm :: TreacheryCard EvilStorm
evilStorm = treachery EvilStorm Cards.evilStorm
