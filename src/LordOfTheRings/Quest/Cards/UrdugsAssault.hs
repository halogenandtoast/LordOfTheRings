module LordOfTheRings.Quest.Cards.UrdugsAssault (urdugsAssault) where

import LordOfTheRings.Card (CardType (EnemyType))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland.Helpers (goblinsAssaultProgress, placeAssaultProgress)

newtype UrdugsAssault = UrdugsAssault QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

urdugsAssault :: QuestCard UrdugsAssault
urdugsAssault = quest UrdugsAssault Cards.urdugsAssault

-- | Forced: At the end of the round, place 1 progress on The Goblins' Assault for each enemy
-- in the staging area. Then, if there is X or more progress on The Goblins' Assault,
-- advance. X is equal to 2, plus 2 per player.
instance HasAbilities UrdugsAssault where
  getAbilities (UrdugsAssault a) = [mkAbility a 1 $ forced RoundEndsWindow | a.flipped]

{- | Setup: Add The Goblins' Assault, The Heroes' Defense, and Urdug to the staging area.
Shuffle the encounter deck and discard cards from the top until X enemies are discarded,
where X is the number of players in the game. Add those enemies to the staging area. The
scenario set the three cards aside as it gathered the encounter deck; the cards are
discarded once the shuffled deck is there to discard from.
-}
instance RunMessage UrdugsAssault where
  runMessage msg q@(UrdugsAssault attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    PerformSetup (isTarget attrs -> True) -> do
      addSetAsideToStaging $ CardMatchAny [cardIs Objectives.theGoblinsAssault, cardIs Objectives.theHeroesDefense, cardIs Enemies.urdug]
      shuffleEncounterDeck
      doStep 1 msg
      UrdugsAssault <$> runMessage msg attrs
    DoStep 1 (PerformSetup (isTarget attrs -> True)) -> do
      players <- length <$> getRemainingPlayers
      discardEncounterCardsUntilCount attrs players (CardWithType EnemyType)
      pure q
    DiscardedTopOfEncounterDeck (isSource attrs -> True) _ cards -> do
      for_ (filter (cardMatches $ CardWithType EnemyType) cards) addToStagingFromDiscard
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeAssaultProgress attrs . length =<< select EnemyInStagingArea
      doStep 1 msg
      pure q
    DoStep 1 (UseCardAbility _ (isSource attrs -> True) 1 _) -> do
      players <- length <$> getRemainingPlayers
      progress <- goblinsAssaultProgress
      when (progress >= 2 + 2 * players) $ completeQuest attrs.id
      pure q
    _ -> UrdugsAssault <$> runMessage msg attrs
