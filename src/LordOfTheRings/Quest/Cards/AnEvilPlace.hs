module LordOfTheRings.Quest.Cards.AnEvilPlace (anEvilPlace) where

import LordOfTheRings.Card (CardType (EncounterSideQuestType, EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains.Helpers

newtype AnEvilPlace = AnEvilPlace QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

anEvilPlace :: QuestCard AnEvilPlace
anEvilPlace = quest AnEvilPlace Cards.anEvilPlace

-- | Event cards cannot shuffle cards from a player's discard pile into his deck.
instance HasModifiersFor AnEvilPlace where
  getModifiersFor (AnEvilPlace a) = dwindlingSuppliesModifiers a

-- | Forced: At the end of the round, each player discards X cards from the top of his
-- deck, where X is 1 more than the number of quest cards in play. If a player has no cards
-- left in his deck, he is eliminated from the game.
instance HasAbilities AnEvilPlace where
  getAbilities (AnEvilPlace a) = dwindlingSupplies a

{- | When Revealed: Search the encounter deck and discard pile for an encounter side quest
and reveal it. Shuffle the encounter discard pile into the encounter deck and discard cards
from the top until X enemies are discarded, where X is the number of players. Add each
enemy discarded this way to the staging area.

The first player chooses the side quest. The shuffled deck is read a step later. If the
players defeat this stage, they win the game: it is the last.
-}
instance RunMessage AnEvilPlace where
  runMessage msg q@(AnEvilPlace attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      AnEvilPlace <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscardToReveal lead attrs (CardWithType EncounterSideQuestType)
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard (filter (cardMatches $ CardWithType EnemyType) cards)
      pure q
    _ -> do
      runDwindlingSupplies msg attrs
      AnEvilPlace <$> runMessage msg attrs
