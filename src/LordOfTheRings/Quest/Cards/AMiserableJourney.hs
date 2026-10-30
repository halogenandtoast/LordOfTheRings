module LordOfTheRings.Quest.Cards.AMiserableJourney (aMiserableJourney) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype AMiserableJourney = AMiserableJourney QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aMiserableJourney :: QuestCard AMiserableJourney
aMiserableJourney = quest AMiserableJourney Cards.aMiserableJourney

-- | Reveal 1 additional encounter card during the quest phase unless there is an active
-- location with the safe keyword.
instance HasModifiersFor AMiserableJourney where
  getModifiersFor (AMiserableJourney a) =
    unlessM safeIsActive $ modified a GameTarget [AdditionalStagingReveals 1]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck. Discard cards
-- from the top of the encounter deck until a side quest is discarded. Reveal that side
-- quest and add it to the staging area, if able.
instance RunMessage AMiserableJourney where
  runMessage msg q@(AMiserableJourney attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      AMiserableJourney <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardUntilSideQuest attrs
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ revealFromEncounterDiscard (sideQuestsAmong cards)
      pure q
    _ -> AMiserableJourney <$> runMessage msg attrs
