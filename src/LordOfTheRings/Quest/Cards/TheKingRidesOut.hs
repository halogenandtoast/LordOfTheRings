module LordOfTheRings.Quest.Cards.TheKingRidesOut (theKingRidesOut) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (addDiscardedEnemiesToStaging, discardUntilEnemiesPerPlayer)

newtype TheKingRidesOut = TheKingRidesOut QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theKingRidesOut :: QuestCard TheKingRidesOut
theKingRidesOut = quest TheKingRidesOut Cards.theKingRidesOut

-- | Skip the quest phase. Players cannot reduce their threat.
instance HasModifiersFor TheKingRidesOut where
  getModifiersFor (TheKingRidesOut a) = when a.flipped do
    modified a GameTarget [SkipQuestPhase]
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotReduceThreat]

{- | When Revealed: Shuffle the encounter discard pile into the encounter deck. Discard
cards from the encounter deck until X enemies are discarded. X is the number of players.
Add each enemy discarded this way to the staging area.

If there are no enemies in play, the players win the game: checked once the When Revealed
effect has resolved, and after each enemy leaves play.
-}
instance RunMessage TheKingRidesOut where
  runMessage msg q@(TheKingRidesOut attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      TheKingRidesOut <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 revealed@(WhenRevealed (isTarget attrs -> True)) -> do
      discardUntilEnemiesPerPlayer attrs
      doStep 2 revealed
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      addDiscardedEnemiesToStaging cards
      pure q
    DoStep 2 inner | attrs.flipped, checks inner -> do
      none <- selectNone AnyEnemy
      when none winGame
      pure q
    _ | attrs.flipped, isJust (removedEnemy msg) -> do
      doStep 2 msg
      TheKingRidesOut <$> runMessage msg attrs
    _ -> TheKingRidesOut <$> runMessage msg attrs
   where
    checks = \case
      WhenRevealed t -> isTarget attrs t
      m -> isJust (removedEnemy m)
