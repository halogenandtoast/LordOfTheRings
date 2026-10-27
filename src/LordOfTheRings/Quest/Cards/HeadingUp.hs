module LordOfTheRings.Quest.Cards.HeadingUp (headingUp) where

import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype HeadingUp = HeadingUp QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

headingUp :: QuestCard HeadingUp
headingUp = quest HeadingUp Cards.headingUp

-- | Forced: If Heading Down is in the players' victory display at the end of any quest
-- phase, shuffle Heading Down back into the quest deck.
instance HasAbilities HeadingUp where
  getAbilities (HeadingUp a) =
    searchForAnExit a
      <> bypass a
      <> [ restricted a 1 (CardInVictoryDisplay $ cardIs Cards.headingDown) $ forced $ PhaseEndsWindow When QuestPhase
         | a.flipped
         ]

instance RunMessage HeadingUp where
  runMessage msg q@(HeadingUp attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      shuffleBackFromVictoryDisplay Cards.headingDown
      pure q
    _ -> HeadingUp <$> runSearchForAnExit msg attrs
