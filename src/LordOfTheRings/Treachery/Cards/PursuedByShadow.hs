module LordOfTheRings.Treachery.Cards.PursuedByShadow (pursuedByShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PursuedByShadow = PursuedByShadow TreacheryAttrs
  deriving anyclass (IsTreachery, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

pursuedByShadow :: TreacheryCard PursuedByShadow
pursuedByShadow = treachery PursuedByShadow Cards.pursuedByShadow
