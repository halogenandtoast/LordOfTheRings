module LordOfTheRings.Quest.Cards.TheTurnaround (theTurnaround) where

import LordOfTheRings.Card (RacingResult (Turn))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers

newtype TheTurnaround = TheTurnaround QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTurnaround :: QuestCard TheTurnaround
theTurnaround = quest TheTurnaround Cards.theTurnaround

-- | Race. This stage gets +4 quest points per player.
instance HasModifiersFor TheTurnaround where
  getModifiersFor (TheTurnaround a) = raceStageModifiers a

-- | Forced: When this stage is defeated, make a racing Willpower test. To pass requires 1
-- TURN result, plus an additional TURN result for each stage you have completed this
-- round. If the players pass the test, excess progress is placed on the next stage. If the
-- players fail the test, each player discards an ally he controls.
instance RunMessage TheTurnaround where
  runMessage msg (TheTurnaround attrs) = TheTurnaround <$> runRaceStage EscapeWillpower Turn failure msg attrs
   where
    failure =
      StillDefeated $ eachPlayer \p -> do
        allies <- select (AllyControlledBy $ PlayerWithId p)
        chooseOrRunOneM p do
          questionLabeled "theTurnaround.discardAlly"
          causedBy attrs
          targets allies discard
