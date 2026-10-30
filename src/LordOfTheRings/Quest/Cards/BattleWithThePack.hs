module LordOfTheRings.Quest.Cards.BattleWithThePack (battleWithThePack) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (discardUntilWargs, flipToNight)

newtype BattleWithThePack = BattleWithThePack QuestAttrs
  deriving anyclass (IsQuest)
  deriving newtype (Show, Eq, ToJSON, Entity)

battleWithThePack :: QuestCard BattleWithThePack
battleWithThePack = quest BattleWithThePack Cards.battleWithThePack

packLeader :: EnemyMatcher
packLeader = EnemyWithCardCode (toCardCode Enemies.packLeader)

-- | Pack Leader cannot take damage unless there are at least 5 progress tokens on this
-- stage.
instance HasModifiersFor BattleWithThePack where
  getModifiersFor (BattleWithThePack a) = when (a.flipped && a.progress < 5) do
    leaders <- select packLeader
    for_ leaders \e -> modified a e [CannotTakeDamage]

-- | Forced: When it becomes Day, discard all progress from this stage.
instance HasAbilities BattleWithThePack where
  getAbilities (BattleWithThePack a) = [mkAbility a 1 $ forced $ BecomesWindow Daytime | a.flipped]

-- | When Revealed: Flip the Time objective to Nightfall. Add Pack Leader to the staging
-- area. Shuffle the encounter discard pile into the encounter deck and discard cards from
-- the top of the encounter deck until X Warg enemies are discarded. X is 1 less than the
-- number of players in the game. Add each discarded enemy to the staging area.
-- The players cannot defeat this stage while Pack Leader is in play. If Pack Leader is
-- destroyed, the players win the game.
instance RunMessage BattleWithThePack where
  runMessage msg q@(BattleWithThePack attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      stage
    WhenRevealed (isTarget attrs -> True) -> do
      flipToNight
      addSetAsideToStaging (cardIs Enemies.packLeader)
      shuffleEncounterDiscardIntoDeck AnyCard
      doStep 1 msg
      pure q
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      discardUntilWargs . subtract 1 . length =<< getRemainingPlayers
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      when (attrs.progress > 0) $ removeTokens attrs attrs Progress attrs.progress
      pure q
    Destroy _ (EnemyTarget eid) -> do
      whenM (selectAny $ EnemyWithId eid <> packLeader) winGame
      stage
    _ -> stage
   where
    stage = BattleWithThePack <$> runBlockedStage packLeader msg attrs
