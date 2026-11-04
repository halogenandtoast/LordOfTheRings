module LordOfTheRings.Quest.Cards.FleeTheCity (fleeTheCity) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EscapeFromUmbar.Helpers (fleeing)

newtype FleeTheCity = FleeTheCity QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

fleeTheCity :: QuestCard FleeTheCity
fleeTheCity = quest FleeTheCity Cards.fleeTheCity

-- | This stage cannot have more than 15 progress tokens on it. While at least 1 player is
-- engaged with an enemy, the players cannot defeat this stage. If the players defeat this
-- stage, they win the game: it is the last stage.
instance HasModifiersFor FleeTheCity where
  getModifiersFor (FleeTheCity a) = fleeing a

-- | Forced: At the beginning of the quest phase, if any player is not engaged with an
-- enemy, discard cards from the top of the encounter deck until an enemy is discarded and
-- add it to the staging area.
instance HasAbilities FleeTheCity where
  getAbilities (FleeTheCity a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After QuestPhase | a.flipped]

enemyCard :: CardMatcher
enemyCard = CardWithType EnemyType

{- | When Revealed: End the current phase. Shuffle the encounter discard pile into the
encounter deck. Starting with the first player, each player discards cards from the top of
the encounter deck until he discards an enemy, and puts that enemy into play engaged with
him.
-}
instance RunMessage FleeTheCity where
  runMessage msg q@(FleeTheCity attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      FleeTheCity <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      endCurrentPhase
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (drop (n - 1) players & take 1) \pid -> do
        discardEncounterCardsUntilFor attrs (PlayerTarget pid) enemyCard
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) (PlayerTarget pid) cards -> do
      for_ (find (cardMatches enemyCard) cards) \card -> do
        removeFromEncounterDiscard card
        putEnemyIntoPlayEngaged pid card
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      players <- getRemainingPlayers
      unengaged <- filterM (fmap not . selectAny . EnemyEngagedWith . PlayerWithId) players
      unless (null unengaged) $ discardEncounterCardsUntil attrs enemyCard
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ find (cardMatches enemyCard) cards
      pure q
    _ -> FleeTheCity <$> runMessage msg attrs
