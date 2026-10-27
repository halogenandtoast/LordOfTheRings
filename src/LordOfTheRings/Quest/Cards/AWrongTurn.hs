module LordOfTheRings.Quest.Cards.AWrongTurn (aWrongTurn) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria.Helpers

newtype AWrongTurn = AWrongTurn QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aWrongTurn :: QuestCard AWrongTurn
aWrongTurn = quest AWrongTurn Cards.aWrongTurn

instance HasAbilities AWrongTurn where
  getAbilities (AWrongTurn a) = searchForAnExit a <> bypass a

-- | When Revealed: Reveal 1 encounter card per player, and add it to the staging area.
instance RunMessage AWrongTurn where
  runMessage msg (AWrongTurn attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      eachPlayer $ const revealEncounterCard
      AWrongTurn <$> runSearchForAnExit msg attrs
    _ -> AWrongTurn <$> runSearchForAnExit msg attrs
