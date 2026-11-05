module LordOfTheRings.Quest.Cards.BattleInTheBay (battleInTheBay) where

import LordOfTheRings.Card (CardType (EnemyType), IsCard (toCard))
import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (guardedObjectives, notGuarding, raiderFlagship, unguardedObjectives)
import LordOfTheRings.Trait (Trait (Ship))

newtype BattleInTheBay = BattleInTheBay QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

battleInTheBay :: QuestCard BattleInTheBay
battleInTheBay = quest BattleInTheBay Cards.battleInTheBay

-- | If your current heading is at the worst setting, Raider Flagship gets -50 engagement
-- cost.
instance HasModifiersFor BattleInTheBay where
  getModifiersFor (BattleInTheBay a) = when a.flipped do
    whenM atWorstHeading do
      flagships <- select raiderFlagship
      for_ flagships \f -> modified a f [EngagementCostModifier (-50)]

-- | You cannot advance while at least 1 objective is guarded by an encounter.
noneGuarded :: HasGame m => m Bool
noneGuarded = null <$> guardedObjectives

{- | When Revealed: Search the encounter deck and discard pile for a Ship enemy and add it
to the staging area (2 Ship enemies instead if there are 3 or more players in the game).
Shuffle the encounter deck. For each unguarded objective in the staging area, attach a
non-unique Ship enemy in the staging area to that objective, guarding it.
Sailing.
-}
instance RunMessage BattleInTheBay where
  runMessage msg q@(BattleInTheBay attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      BattleInTheBay <$> runConditionalStage noneGuarded msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      players <- length <$> getRemainingPlayers
      lead <- getLead
      replicateM_ (if players >= 3 then 2 else 1)
        $ searchEncounterDeckAndDiscard lead attrs (CardWithType EnemyType <> CardWithTrait Ship)
      shuffleEncounterDeck
      doStep 1 msg
      pure q
    -- one unguarded objective at a time, while a non-unique Ship enemy is free to guard it
    DoStep 1 inner@(WhenRevealed (isTarget attrs -> True)) -> do
      objectives <- unguardedObjectives
      for_ (take 1 objectives) \o -> do
        ships <- traverse (\e -> (e,) <$> getEnemy e) =<< notGuarding =<< select (shipEnemy <> EnemyInStagingArea <> not_ UniqueEnemy)
        lead <- getLead
        unless (null ships) $ chooseOrRunOneM lead do
          questionLabeled "battleInTheBay.guard"
          causedBy attrs
          for_ ships \(eid, enemy) -> targeting eid do
            guardObjectiveWith o (toCard enemy).id
            doStep 1 inner
      pure q
    -- an encounter guarding an objective leaves play, or stops guarding it
    DoStep 1 (Destroy _ (EnemyTarget _)) -> recheck
    Discard _ (EnemyTarget _) -> recheck
    Flip (EnemyTarget _) -> recheck
    FinishRevelation _ -> recheck
    _ -> BattleInTheBay <$> runConditionalStage noneGuarded msg attrs
   where
    recheck = do
      checkStage attrs.id
      BattleInTheBay <$> runConditionalStage noneGuarded msg attrs
