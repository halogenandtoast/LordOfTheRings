module LordOfTheRings.Quest.Cards.TheRavenClan (theRavenClan) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (addRavenCardToStaging, revealRavenCard, shuffleRavenDiscardIntoDeck)

newtype TheRavenClan = TheRavenClan QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

theRavenClan :: QuestCard TheRavenClan
theRavenClan = quest TheRavenClan Cards.theRavenClan

-- | Each location in the staging area gets +2 quest points.
instance HasModifiersFor TheRavenClan where
  getModifiersFor (TheRavenClan a) = when a.flipped do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [QuestPointModifier 2]

-- | Forced: After the last time counter is removed from this stage, reveal the top card of
-- the Raven deck and add it to the staging area. Place 3 time counters on this stage.
instance HasAbilities TheRavenClan where
  getAbilities (TheRavenClan a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a | a.flipped]

-- | When Revealed: Shuffle the Raven deck discard pile into the Raven deck. Then, starting
-- with the first player, each player takes the top card of the Raven deck and adds it to
-- the staging area.
instance RunMessage TheRavenClan where
  runMessage msg q@(TheRavenClan attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheRavenClan <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleRavenDiscardIntoDeck
      eachPlayer $ const addRavenCardToStaging
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      revealRavenCard
      placeTimeCounters attrs attrs 3
      pure q
    _ -> TheRavenClan <$> runMessage msg attrs
