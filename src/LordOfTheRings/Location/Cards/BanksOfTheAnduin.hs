module LordOfTheRings.Location.Cards.BanksOfTheAnduin (banksOfTheAnduin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype BanksOfTheAnduin = BanksOfTheAnduin LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

banksOfTheAnduin :: LocationCard BanksOfTheAnduin
banksOfTheAnduin = location BanksOfTheAnduin Cards.banksOfTheAnduin
