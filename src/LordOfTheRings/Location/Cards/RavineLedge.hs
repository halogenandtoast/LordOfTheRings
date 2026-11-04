module LordOfTheRings.Location.Cards.RavineLedge (ravineLedge) where

import LordOfTheRings.Card (RacingResult (Turn))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers (racingTest)

newtype RavineLedge = RavineLedge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravineLedge :: LocationCard RavineLedge
ravineLedge = location RavineLedge Cards.ravineLedge

-- | While Ravine Ledge is in the staging area, each racing test requires 1 additional
-- matching result.
instance HasModifiersFor RavineLedge where
  getModifiersFor (RavineLedge a) = when (a.placement == StagingArea) $ modified a GameTarget [RacingTestAdditionalResults 1]

{- | Travel: The first player makes a racing Attack test. To pass requires 1 TURN result. If
the players pass the test, each player draws 1 card. If the players fail the test, each
player discards 1 random card from his hand. A Travel cost is paid with the location still
in the staging area (FAQ 3.123), so its own test requires its additional result.
-}
instance RunMessage RavineLedge where
  runMessage msg l@(RavineLedge a) = case msg of
    TravelTo lid | lid == a.id -> do
      racingTest a EscapeAttack Turn 1 (eachPlayer \p -> drawCards a p 1) (eachPlayer (`discardRandomFromHand` 1))
      pure l
    _ -> RavineLedge <$> runMessage msg a
