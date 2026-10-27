module LordOfTheRings.Quest.Cards.GoblinPatrol (goblinPatrol) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype GoblinPatrol = GoblinPatrol QuestAttrs
  deriving anyclass (IsQuest, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinPatrol :: QuestCard GoblinPatrol
goblinPatrol = quest GoblinPatrol Cards.goblinPatrol

-- | Forced: After an enemy is revealed from the encounter deck, discard it instead of
-- adding it to the staging area. (Discarded as it is revealed, so its When Revealed
-- effects do not resolve.)
instance HasAbilities GoblinPatrol where
  getAbilities (GoblinPatrol a) = [mkAbility a 1 $ forced $ EnemyRevealedWindow When]

-- | When Revealed: Each player must search the encounter deck and discard pile for 1 enemy
-- of his choice, and add it to the staging area. One choice must be Patrol Leader, if able.
-- If there are no enemies in play, immediately advance to the next stage of the scenario.
-- (Players can also advance by placing 11 progress tokens on Goblin Patrol.)
instance RunMessage GoblinPatrol where
  runMessage msg q@(GoblinPatrol attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      GoblinPatrol <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      scenario <- getsGame (toAttrs . (.scenario))
      let leaderAvailable =
            any ((== toCardCode Enemies.patrolLeader) . toCardCode) (scenario.encounterDeck <> scenario.encounterDiscard)
      players <- getPlayerOrder
      -- the first player takes Patrol Leader while there is one to take
      for_ (zip [0 :: Int ..] players) \(i, p) ->
        searchEncounterDeckAndDiscard p attrs
          $ if i == 0 && leaderAvailable then cardIs Enemies.patrolLeader else CardWithType EnemyType
      checkStage attrs.id
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [eid | Window _ (Revealed (EnemyTarget eid)) <- ws] (discard . EnemyTarget)
      checkStage attrs.id
      pure q
    CheckStage qid | qid == attrs.id -> do
      whenM (selectNone AnyEnemy) $ push (CompleteQuest attrs.id)
      pure q
    Destroy (EnemyTarget _) -> recheck
    Discard (EnemyTarget _) -> recheck
    ReturnToTopOfEncounterDeck (EnemyTarget _) -> recheck
    ShuffleIntoEncounterDeck (EnemyTarget _) -> recheck
    _ -> GoblinPatrol <$> runMessage msg attrs
   where
    recheck = do
      checkStage attrs.id
      GoblinPatrol <$> runMessage msg attrs
