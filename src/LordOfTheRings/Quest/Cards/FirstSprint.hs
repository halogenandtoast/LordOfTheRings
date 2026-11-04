module LordOfTheRings.Quest.Cards.FirstSprint (firstSprint) where

import LordOfTheRings.Card (RacingResult (Straight))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders.Helpers

newtype FirstSprint = FirstSprint QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

firstSprint :: QuestCard FirstSprint
firstSprint = quest FirstSprint Cards.firstSprint

-- | Race. This stage gets +4 quest points per player.
instance HasModifiersFor FirstSprint where
  getModifiersFor (FirstSprint a) = raceStageModifiers a

-- | Forced: When this stage is defeated, make a racing Attack test. To pass requires 1
-- STRAIGHT result, plus an additional STRAIGHT result for each stage you have completed this
-- round. If the players pass the test, excess progress is placed on the next stage. If the
-- players fail the test, deal 1 damage to each character.
instance RunMessage FirstSprint where
  runMessage msg (FirstSprint attrs) = FirstSprint <$> runRaceStage EscapeAttack Straight failure msg attrs
   where
    failure =
      StillDefeated do
        characters <- selectCharacters AnyHero AnyAlly
        for_ characters \c -> dealDamage attrs c 1
