module LordOfTheRings.Location.Cards.SouthPier (southPier) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype SouthPier = SouthPier LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

southPier :: LocationCard SouthPier
southPier = location SouthPier Cards.southPier

instance HasModifiersFor SouthPier where
  getModifiersFor (SouthPier a) = do
    locations <- select AnyLocation
    for_ (filter (/= a.id) locations) \lid -> modified a lid [CannotBeTraveledTo]
