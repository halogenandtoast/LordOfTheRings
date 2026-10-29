module LordOfTheRings.Quest.Cards.TheTrialsBegin (theTrialsBegin) where

import LordOfTheRings.Frontend
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheTrialsBegin = TheTrialsBegin QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTrialsBegin :: QuestCard TheTrialsBegin
theTrialsBegin = quest TheTrialsBegin Cards.theTrialsBegin

-- | Setup: the scenario sets the Guardians, Keys, Barrows and Hallowed Circle aside as it
-- gathers the encounter deck.
-- When Revealed: The players advance to a stage 2A quest of their choice.
instance RunMessage TheTrialsBegin where
  runMessage msg q@(TheTrialsBegin attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      send $ FlipCard attrs.cardId
      askToResolveWhenRevealed attrs
      pure $ TheTrialsBegin $ attrs {questFlipped = True}
    WhenRevealed (isTarget attrs -> True) -> do
      completeQuest attrs.id
      pure q
    _ -> TheTrialsBegin <$> runMessage msg attrs
