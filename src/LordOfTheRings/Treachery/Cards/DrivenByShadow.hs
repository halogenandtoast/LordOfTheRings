module LordOfTheRings.Treachery.Cards.DrivenByShadow (drivenByShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DrivenByShadow = DrivenByShadow TreacheryAttrs
  deriving anyclass (IsTreachery, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

drivenByShadow :: TreacheryCard DrivenByShadow
drivenByShadow = treachery DrivenByShadow Cards.drivenByShadow
