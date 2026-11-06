module LordOfTheRings.Location.Cards.EastPier (eastPier) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype EastPier = EastPier LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

eastPier :: LocationCard EastPier
eastPier = location EastPier Cards.eastPier

instance HasModifiersFor EastPier where
  getModifiersFor (EastPier a) = do
    modified a GameTarget [WhenRevealedCannotBeCanceled]
