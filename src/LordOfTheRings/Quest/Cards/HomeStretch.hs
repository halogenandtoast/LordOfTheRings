module LordOfTheRings.Quest.Cards.HomeStretch (homeStretch) where

import LordOfTheRings.Card (RacingResult (Turn))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers

newtype HomeStretch = HomeStretch QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

homeStretch :: QuestCard HomeStretch
homeStretch = quest HomeStretch Cards.homeStretch

-- | Race. This stage gets +4 quest points per player.
instance HasModifiersFor HomeStretch where
  getModifiersFor (HomeStretch a) = raceStageModifiers a

-- | Forced: When this stage is defeated, make a racing Attack test. To pass requires 1
-- TURN result, plus an additional TURN result for each stage you have completed this
-- round. If the players pass the test, excess progress is placed on the next stage. If the
-- players fail the test, this stage is not defeated and all progress here is discarded.
instance RunMessage HomeStretch where
  runMessage msg (HomeStretch attrs) = HomeStretch <$> runRaceStage EscapeAttack Turn failure msg attrs
   where
    failure =
      NotDefeated
