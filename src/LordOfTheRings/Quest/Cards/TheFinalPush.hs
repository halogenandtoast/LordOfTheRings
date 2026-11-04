module LordOfTheRings.Quest.Cards.TheFinalPush (theFinalPush) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (goblinsAssaultProgress, placeAssaultProgress)

newtype TheFinalPush = TheFinalPush QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFinalPush :: QuestCard TheFinalPush
theFinalPush = quest TheFinalPush Cards.theFinalPush

-- | Forced: At the end of the round, place 1 progress on The Goblins' Assault for each enemy
-- in play. Then, if there is X or more progress on The Goblins' Assault, the players lose
-- the game. X is equal to 8, plus 8 per player.
instance HasAbilities TheFinalPush where
  getAbilities (TheFinalPush a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

-- | When Revealed: Shuffle the encounter discard pile into the encounter deck and discard
-- cards from the top until X enemies are discarded, where X is the number of players in the
-- game. Add those enemies to the staging area. The shuffled deck is read back in a later
-- step.
instance RunMessage TheFinalPush where
  runMessage msg q@(TheFinalPush attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheFinalPush <$> runMessage msg attrs
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
      placeAssaultProgress attrs . length =<< select AnyEnemy
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      players <- length <$> getRemainingPlayers
      progress <- goblinsAssaultProgress
      when (progress >= 8 + 8 * players) $ gameOver Lost
      pure q
    _ -> TheFinalPush <$> runMessage msg attrs
