module LordOfTheRings.Location.Cards.FortressWallsTheFortressOfNurn (fortressWallsTheFortressOfNurn) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castleSideQuests, placeFromHandUnderCastleSideQuest)

newtype FortressWallsTheFortressOfNurn = FortressWallsTheFortressOfNurn LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fortressWallsTheFortressOfNurn :: LocationCard FortressWallsTheFortressOfNurn
fortressWallsTheFortressOfNurn = location FortressWallsTheFortressOfNurn Cards.fortressWallsTheFortressOfNurn

{- | While Fortress Walls is in the staging area, the cost to play each ally is increased by
1. Its Travel cost must be paid to travel here: not while a player has no card in hand,
nor with no Castle side quest in play to place one under.
-}
instance HasModifiersFor FortressWallsTheFortressOfNurn where
  getModifiersFor (FortressWallsTheFortressOfNurn a) = do
    when (a.placement == StagingArea) $ modified a GameTarget [IncreaseCost (CardWithType AllyType) 1]
    hands <- traverse (fmap (.hand) . getPlayer) =<< getRemainingPlayers
    castles <- castleSideQuests
    when (any null hands || null castles) $ modified a a [CannotBeTraveledTo]

-- | Travel: Each player places 1 card from his hand under a Castle side quest.
instance RunMessage FortressWallsTheFortressOfNurn where
  runMessage msg l@(FortressWallsTheFortressOfNurn a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer $ placeFromHandUnderCastleSideQuest a
      pure l
    _ -> FortressWallsTheFortressOfNurn <$> runMessage msg a
