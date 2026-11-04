module LordOfTheRings.Quest.Cards.GettingCloser (gettingCloser) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (flipSearchTo, isOnTrack)

newtype GettingCloser = GettingCloser QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

gettingCloser :: QuestCard GettingCloser
gettingCloser = quest GettingCloser Cards.gettingCloser

-- | While On Track is in play, encounter card effects cannot be canceled.
instance HasModifiersFor GettingCloser where
  getModifiersFor (GettingCloser a) = when a.flipped $ whenM isOnTrack $ modified a GameTarget [CannotCancelEncounterEffects]

{- | When Revealed (2A): Flip the Search objective to Off Track. Shuffle the encounter
discard pile into the encounter deck and discard cards from the top until X enemies are
discarded, where X is the number of players. Add each enemy discarded by this effect to
the staging area. The shuffled deck is read back in a later step.
-}
instance RunMessage GettingCloser where
  runMessage msg q@(GettingCloser attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      GettingCloser <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      flipSearchTo Objectives.offTrack
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      traverse_ addToStagingFromDiscard $ filter (cardMatches $ CardWithType EnemyType) cards
      pure q
    _ -> GettingCloser <$> runMessage msg attrs
