module LordOfTheRings.Quest.Cards.HeadingDown (headingDown) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype HeadingDown = HeadingDown QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

headingDown :: QuestCard HeadingDown
headingDown = quest HeadingDown Cards.headingDown

-- | Forced: If Heading Up is in the players' victory display at the end of any quest
-- phase, shuffle Heading Up back into the quest deck.
instance HasAbilities HeadingDown where
  getAbilities (HeadingDown a) =
    searchForAnExit a
      <> bypass a
      <> [ restricted a 1 (CardInVictoryDisplay $ cardIs Cards.headingUp) $ forced $ PhaseEndsWindow When QuestPhase
         | a.flipped
         ]

instance RunMessage HeadingDown where
  runMessage msg q@(HeadingDown attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleBackFromVictoryDisplay Cards.headingUp
      pure q
    _ -> HeadingDown <$> runSearchForAnExit msg attrs
