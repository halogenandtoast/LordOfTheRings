module LordOfTheRings.Quest.Cards.AssaultOnHelmsDeep (assaultOnHelmsDeep) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.HelmsDeep.Helpers (firstPlayerReveals)

newtype AssaultOnHelmsDeep = AssaultOnHelmsDeep QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Defense, as its card def says.
assaultOnHelmsDeep :: QuestCard AssaultOnHelmsDeep
assaultOnHelmsDeep = quest AssaultOnHelmsDeep Cards.assaultOnHelmsDeep

{- | Forced: At the end of the combat phase, if the number of enemies in the staging area
is less than the number of players in the game, the first player reveals an encounter
card.

Forced: At the end of the round place 1 progress here for each enemy in the staging area
(bypassing any active location).
-}
instance HasAbilities AssaultOnHelmsDeep where
  getAbilities (AssaultOnHelmsDeep a) =
    [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase | a.flipped]
      <> [mkAbility a 2 $ forced RoundEndsWindow | a.flipped]

{- | When Revealed: Add The Defense of Helm's Deep and each set aside copy of Soldier of
Isengard to the staging area. Make Deeping Wall the active location. Shuffle the encounter
deck. Then, each player reveals 1 encounter card, as its revealer.
-}
instance RunMessage AssaultOnHelmsDeep where
  runMessage msg q@(AssaultOnHelmsDeep attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      AssaultOnHelmsDeep <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Objectives.theDefenseOfHelmsDeep)
      addSetAsideToStaging (cardIs Enemies.soldierOfIsengard)
      traverse_ putIntoPlayAsActiveLocation . take 1 =<< getSetAside (cardIs Locations.deepingWall)
      shuffleEncounterDeck
      eachPlayer revealEncounterCardAs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      enemies <- length <$> select EnemyInStagingArea
      players <- length <$> getRemainingPlayers
      when (enemies < players) firstPlayerReveals
      pure q
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      enemies <- length <$> select EnemyInStagingArea
      when (enemies > 0) $ placeProgress attrs attrs enemies
      pure q
    _ -> AssaultOnHelmsDeep <$> runMessage msg attrs
