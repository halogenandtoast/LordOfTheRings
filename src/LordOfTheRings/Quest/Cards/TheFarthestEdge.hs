module LordOfTheRings.Quest.Cards.TheFarthestEdge (theFarthestEdge) where

import LordOfTheRings.Card (RacingResult (Straight))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers

newtype TheFarthestEdge = TheFarthestEdge QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFarthestEdge :: QuestCard TheFarthestEdge
theFarthestEdge = quest TheFarthestEdge Cards.theFarthestEdge

-- | Race. This stage gets +4 quest points per player.
instance HasModifiersFor TheFarthestEdge where
  getModifiersFor (TheFarthestEdge a) = raceStageModifiers a

-- | Forced: When this stage is defeated, make a racing Defense test. To pass requires 1
-- STRAIGHT result, plus an additional STRAIGHT result for each stage you have completed this
-- round. If the players pass the test, excess progress is placed on the next stage. If the
-- players fail the test, reveal the top card of the encounter deck (top 2 cards instead if there are 3 or more players).
instance RunMessage TheFarthestEdge where
  runMessage msg (TheFarthestEdge attrs) = TheFarthestEdge <$> runRaceStage EscapeDefense Straight failure msg attrs
   where
    failure =
      StillDefeated do
        players <- length <$> getRemainingPlayers
        revealEncounterCards (if players >= 3 then 2 else 1)
