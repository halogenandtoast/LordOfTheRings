module LordOfTheRings.Location.Cards.HillsideManor (hillsideManor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype HillsideManor = HillsideManor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hillsideManor :: LocationCard HillsideManor
hillsideManor = location HillsideManor Cards.hillsideManor

-- | While Hillside Manor is in the staging area, each player's threat cannot be reduced by
-- more than 1 each round by player card effects. What player cards reduced it this round
-- counts, from before it arrived too.
instance HasModifiersFor HillsideManor where
  getModifiersFor (HillsideManor a) = when (a.placement == StagingArea) do
    players <- getRemainingPlayers
    for_ players \p -> do
      reduced <- getThreatReducedByPlayerCardsThisRound p
      modified a (PlayerTarget p) [PlayerCardThreatReductionLimit (max 0 $ 1 - reduced)]

-- | Travel: Each player discards 1 random card from his hand.
instance RunMessage HillsideManor where
  runMessage msg (HillsideManor a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (`discardRandomFromHand` 1)
      HillsideManor <$> runMessage msg a
    _ -> HillsideManor <$> runMessage msg a
