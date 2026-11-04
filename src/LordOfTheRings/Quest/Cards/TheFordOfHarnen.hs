module LordOfTheRings.Quest.Cards.TheFordOfHarnen (theFordOfHarnen) where

import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers

newtype TheFordOfHarnen = TheFordOfHarnen QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFordOfHarnen :: QuestCard TheFordOfHarnen
theFordOfHarnen = quest TheFordOfHarnen Cards.theFordOfHarnen

{- | Forced: After characters are committed to quest, no more than 5 progress can be placed
here this round (10 progress instead if each Tamed Mumak in play is committed to the
quest). See 'limitProgressThisRound'.
Forced: After an enemy attacks and destroys a character, place 1 progress on the Orcs'
stage.
If you defeat this stage, you have escaped the Orcs and win the game: it is the last.
-}
instance HasAbilities TheFordOfHarnen where
  getAbilities (TheFordOfHarnen a) =
    [mkAbility a 1 $ forced StagingStepBeginsWindow | a.flipped]
      <> [mkAbility a 2 $ forced $ CharacterDestroyedByAttackWindow After Nothing | a.flipped]

-- | When Revealed: Add River Harnen to the staging area. Shuffle the encounter discard pile
-- into the encounter deck and discard cards from the top until an Orc enemy is discarded.
-- Add that enemy to the Orcs' area.
instance RunMessage TheFordOfHarnen where
  runMessage msg q@(TheFordOfHarnen attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      TheFordOfHarnen <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Locations.riverHarnen)
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardEncounterCardsUntil attrs orcEnemy
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      addOrcToOrcsArea cards
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      limitProgressThisRound attrs
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      placeOrcsProgress 1
      pure q
    _ -> TheFordOfHarnen <$> runMessage msg attrs
