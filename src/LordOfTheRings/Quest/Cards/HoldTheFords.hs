module LordOfTheRings.Quest.Cards.HoldTheFords (holdTheFords) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers

newtype HoldTheFords = HoldTheFords QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

holdTheFords :: QuestCard HoldTheFords
holdTheFords = quest HoldTheFords Cards.holdTheFords

-- | Time 3. Forced: After the last time counter is removed from this stage, discard the
-- top X cards from the encounter deck. X is the number of cards in the first player's
-- hand. Add each enemy discarded this way to the staging area. Put 3 time counters on this
-- stage.
instance HasAbilities HoldTheFords where
  getAbilities (HoldTheFords a) = [mkAbility a 1 $ forced $ lastTimeCounterRemoved a | a.flipped]

-- | When Revealed: Each player searches the encounter deck and discard pile for 1
-- different Dunland enemy and adds it to the staging area. Shuffle the encounter deck.
-- While there is at least 1 enemy in play, the players cannot defeat this stage. If the
-- players defeat this stage, they win the game.
instance RunMessage HoldTheFords where
  runMessage msg q@(HoldTheFords attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      HoldTheFords <$> runBlockedStage AnyEnemy msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      searchForDifferentDunlandEnemies attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      discardTopOfEncounterDeck attrs =<< handSize =<< getLead
      placeTimeCounters attrs attrs 3
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    _ -> HoldTheFords <$> runBlockedStage AnyEnemy msg attrs
