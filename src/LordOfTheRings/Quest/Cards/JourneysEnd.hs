module LordOfTheRings.Quest.Cards.JourneysEnd (journeysEnd) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype JourneysEnd = JourneysEnd QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

journeysEnd :: QuestCard JourneysEnd
journeysEnd = quest JourneysEnd Cards.journeysEnd

-- | Each damaged character gets -2 willpower unless there is an active location with the
-- safe keyword.
-- If the players defeat this stage, they have escaped the Ettenmoors with their lives, and
-- win the game: it is the last stage.
instance HasModifiersFor JourneysEnd where
  getModifiersFor (JourneysEnd a) = unlessM safeIsActive do
    damaged <- damagedCharacters AnyHero AnyAlly
    for_ damaged \c -> modified a c [WillpowerModifier (-2)]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck. Discard cards
-- from the top of the encounter deck until a side quest is discarded. Reveal that side
-- quest and add it to the staging area, if able.
instance RunMessage JourneysEnd where
  runMessage msg q@(JourneysEnd attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      JourneysEnd <$> runMessage msg attrs
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
    _ -> JourneysEnd <$> runMessage msg attrs
