module LordOfTheRings.Quest.Cards.TheWatersEdge (theWatersEdge) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheWatersEdge = TheWatersEdge QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWatersEdge :: QuestCard TheWatersEdge
theWatersEdge = quest TheWatersEdge Cards.theWatersEdge

-- | Forced: After a player commits characters to the quest, he must discard the top 2
-- cards of his deck.
instance HasAbilities TheWatersEdge where
  getAbilities (TheWatersEdge a) = [mkAbility a 1 $ forced $ AnyCommittedToQuestWindow After]

instance RunMessage TheWatersEdge where
  runMessage msg q@(TheWatersEdge attrs) = case msg of
    -- a player's characters are committed together, in one window
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      players <- nub . catMaybes <$> traverse characterController [c | Window _ (CommittedToQuest c) <- windows]
      for_ players \pid -> discardTopOfDeck pid 2
      pure q
    _ -> TheWatersEdge <$> runMessage msg attrs
