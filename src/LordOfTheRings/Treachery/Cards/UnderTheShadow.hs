module LordOfTheRings.Treachery.Cards.UnderTheShadow (underTheShadow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnderTheShadow = UnderTheShadow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

underTheShadow :: TreacheryCard UnderTheShadow
underTheShadow = treachery UnderTheShadow Cards.underTheShadow
