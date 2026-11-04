module LordOfTheRings.Location.Cards.MinesOfCirithGurat (minesOfCirithGurat) where

import LordOfTheRings.Card (CardType (AllyType), cdCardType, toCardDef)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MinesOfCirithGurat = MinesOfCirithGurat LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

minesOfCirithGurat :: LocationCard MinesOfCirithGurat
minesOfCirithGurat = location MinesOfCirithGurat Cards.minesOfCirithGurat

-- | While Mines of Cirith Gurat is in the staging area, it gains: "Forced: When a player
-- card would reduce a player's threat by any amount, reduce that amount to 1."
instance HasModifiersFor MinesOfCirithGurat where
  getModifiersFor (MinesOfCirithGurat a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [PlayerCardThreatReductionLimit 1]

-- | Travel: Each player discards the top 3 cards of his deck and raises his threat by 1 for
-- each ally he discards this way.
instance RunMessage MinesOfCirithGurat where
  runMessage msg (MinesOfCirithGurat a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer \p -> do
        top <- take 3 . (.deck) <$> getPlayer p
        discardTopOfDeck p 3
        encounterRaisesThreat p $ length [c | c <- top, cdCardType (toCardDef c) == AllyType]
      MinesOfCirithGurat <$> runMessage msg a
    _ -> MinesOfCirithGurat <$> runMessage msg a
