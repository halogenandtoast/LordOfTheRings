module LordOfTheRings.Treachery.Cards.Despair (despair) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Despair = Despair TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

despair :: TreacheryCard Despair
despair = treachery Despair Cards.despair
