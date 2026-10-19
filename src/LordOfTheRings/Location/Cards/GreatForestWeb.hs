module LordOfTheRings.Location.Cards.GreatForestWeb (greatForestWeb) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GreatForestWeb = GreatForestWeb LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatForestWeb :: LocationCard GreatForestWeb
greatForestWeb = location GreatForestWeb Cards.greatForestWeb
