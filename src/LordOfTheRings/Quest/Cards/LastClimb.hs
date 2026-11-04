module LordOfTheRings.Quest.Cards.LastClimb (lastClimb) where

import LordOfTheRings.Card (RacingResult (Straight))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers

newtype LastClimb = LastClimb QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

lastClimb :: QuestCard LastClimb
lastClimb = quest LastClimb Cards.lastClimb

-- | Race. This stage gets +4 quest points per player.
instance HasModifiersFor LastClimb where
  getModifiersFor (LastClimb a) = raceStageModifiers a

-- | Forced: When this stage is defeated, make a racing Willpower test. To pass requires 1
-- STRAIGHT result, plus an additional STRAIGHT result for each stage you have completed this
-- round. If the players pass the test, excess progress is placed on the next stage. If the
-- players fail the test, place 2 damage per player on The Wainriders' stage.
instance RunMessage LastClimb where
  runMessage msg (LastClimb attrs) = LastClimb <$> runRaceStage EscapeWillpower Straight failure msg attrs
   where
    failure =
      StillDefeated do
        players <- length <$> getRemainingPlayers
        placeWainridersDamage (2 * players)
