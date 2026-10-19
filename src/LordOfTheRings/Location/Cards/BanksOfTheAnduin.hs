module LordOfTheRings.Location.Cards.BanksOfTheAnduin (banksOfTheAnduin) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype BanksOfTheAnduin = BanksOfTheAnduin LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

banksOfTheAnduin :: LocationCard BanksOfTheAnduin
banksOfTheAnduin = location BanksOfTheAnduin Cards.banksOfTheAnduin

-- | Forced: If Banks of the Anduin leaves play, return it to the top of the encounter deck
-- instead of placing it in the discard pile.
instance HasModifiersFor BanksOfTheAnduin where
  getModifiersFor (BanksOfTheAnduin a) = modified a a [LeavesPlayToTopOfEncounterDeck]
