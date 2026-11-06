module LordOfTheRings.Location.Cards.WestPier (westPier) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Burn))

newtype WestPier = WestPier LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

westPier :: LocationCard WestPier
westPier = location WestPier Cards.westPier

instance HasModifiersFor WestPier where
  getModifiersFor (WestPier a) = do
    locations <- select AnyLocation
    for_ locations \lid -> modified a lid [GainKeyword (Burn 1)]
