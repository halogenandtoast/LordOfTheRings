module LordOfTheRings.Location.Cards.CaveEntranceWesternLands (caveEntranceWesternLands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype CaveEntranceWesternLands = CaveEntranceWesternLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

caveEntranceWesternLands :: LocationCard CaveEntranceWesternLands
caveEntranceWesternLands = location CaveEntranceWesternLands Cards.caveEntranceWesternLands

-- | While Cave Entrance is in the staging area, it gains: "Forced: At the end of the round,
-- place the top X cards of the encounter discard pile on the bottom of the encounter deck.
-- X is the number of players in the game."
instance HasAbilities CaveEntranceWesternLands where
  getAbilities (CaveEntranceWesternLands a) = [mkAbility a 1 $ forced RoundEndsWindow | a.placement == StagingArea]

instance RunMessage CaveEntranceWesternLands where
  runMessage msg l@(CaveEntranceWesternLands a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      players <- length <$> getRemainingPlayers
      moveDiscardToBottomOfEncounterDeck players
      pure l
    _ -> CaveEntranceWesternLands <$> runMessage msg a
