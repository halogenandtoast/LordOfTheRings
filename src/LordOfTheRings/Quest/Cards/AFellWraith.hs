module LordOfTheRings.Quest.Cards.AFellWraith (aFellWraith) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyHitPoints)
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (thaurdirMatcher)

newtype AFellWraith = AFellWraith QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

aFellWraith :: QuestCard AFellWraith
aFellWraith = quest AFellWraith Cards.aFellWraith

-- | If a player has no cards in his deck, he is eliminated from the game.
instance HasModifiersFor AFellWraith where
  getModifiersFor (AFellWraith a) = when a.flipped $ modified a GameTarget [EliminatedWithEmptyDeck]

-- | When Revealed: Add Thaurdir to the staging area. Then, reveal 1 encounter card per
-- player.
-- Forced: After Thaurdir attacks and destroys a character, that character's controller
-- must either discard the top 3 cards of his deck, or return Thaurdir to the staging area.
-- Read off the window after the character is destroyed.
-- The players cannot defeat this stage while Thaurdir has any hit points remaining. If the
-- players defeat this stage, they win the game: it is the last.
instance RunMessage AFellWraith where
  runMessage msg q@(AFellWraith attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      AFellWraith <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      addSetAsideToStaging (cardIs Enemies.thaurdirDeadmensDike)
      revealEncounterCards . length =<< getRemainingPlayers
      pure q
    CheckWindows windows | attrs.flipped -> do
      thaurdir <- selectOne thaurdirMatcher
      for_ [(eid, pid) | Just eid <- [thaurdir], Window After (CharacterDestroyedByAttack eid' _ pid) <- windows, eid' == eid] \(eid, pid) ->
        chooseOneM pid do
          questionLabeled "aFellWraith.choose"
          causedBy attrs
          labeled "aFellWraith.discard" $ discardTopOfDeck pid 3
          labeled "aFellWraith.returnThaurdir" $ placeEnemy eid StagingArea
      stage
    -- damage on Thaurdir may leave him no hit points
    PlaceTokens _ (EnemyTarget _) Damage _ -> do
      checkStage attrs.id
      stage
    _ -> stage
   where
    stage = AFellWraith <$> runConditionalStage thaurdirDown msg attrs
    thaurdirDown =
      selectOne thaurdirMatcher >>= \case
        Nothing -> pure True
        Just eid -> (>=) <$> getDamage (toTarget eid) <*> getEnemyHitPoints eid
