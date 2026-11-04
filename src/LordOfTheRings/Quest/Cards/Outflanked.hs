module LordOfTheRings.Quest.Cards.Outflanked (outflanked) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (goblinsAssaultProgress, placeAssaultProgress)

newtype Outflanked = Outflanked QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

outflanked :: QuestCard Outflanked
outflanked = quest Outflanked Cards.outflanked

-- | Forced: After an enemy attacks, place 1 progress on The Goblins' Assault.
-- Forced: At the end of the round, if there is X or more progress on The Goblins' Assault,
-- advance. X is equal to 4, plus 4 per player.
instance HasAbilities Outflanked where
  getAbilities (Outflanked a) =
    [mkAbility a 1 $ forced $ AnyEnemyAttacksWindow After | a.flipped]
      <> [mkAbility a 2 $ forced RoundEndsWindow | a.flipped]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
-- cards from the top until X enemies are discarded, where X is the number of players in the
-- game. Add those enemies to the staging area. The shuffled deck is read back in a later
-- step.
instance RunMessage Outflanked where
  runMessage msg q@(Outflanked attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      Outflanked <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeAssaultProgress attrs 1
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      players <- length <$> getRemainingPlayers
      progress <- goblinsAssaultProgress
      when (progress >= 4 + 4 * players) $ completeQuest attrs.id
      pure q
    _ -> Outflanked <$> runMessage msg attrs
