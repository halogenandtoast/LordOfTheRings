module LordOfTheRings.Objective.Cards.CoveredInDarkness (coveredInDarkness) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

-- | Of the Power of Mordor deck (The Temple of Doom, The Fortress of Nurn).
newtype CoveredInDarkness = CoveredInDarkness ObjectiveAttrs
  deriving anyclass (IsObjective, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

coveredInDarkness :: ObjectiveCard CoveredInDarkness
coveredInDarkness = objective CoveredInDarkness Cards.coveredInDarkness

-- | Doomed 5.
-- Each player's threat cannot be reduced by more than 1 each round by player card
-- effects. What player cards reduced it this round counts, from before it arrived too.
instance HasModifiersFor CoveredInDarkness where
  getModifiersFor (CoveredInDarkness a) = do
    players <- getRemainingPlayers
    for_ players \p -> do
      reduced <- getThreatReducedByPlayerCardsThisRound p
      modified a (PlayerTarget p) [PlayerCardThreatReductionLimit (max 0 $ 1 - reduced)]

instance RunMessage CoveredInDarkness where
  runMessage msg (CoveredInDarkness a) = CoveredInDarkness <$> runMessage msg a
