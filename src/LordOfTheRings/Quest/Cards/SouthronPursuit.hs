module LordOfTheRings.Quest.Cards.SouthronPursuit (southronPursuit) where

import LordOfTheRings.EncounterSet (EncounterSet (HaradSoldiers))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (runStage3A)

newtype SouthronPursuit = SouthronPursuit QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

southronPursuit :: QuestCard SouthronPursuit
southronPursuit = quest SouthronPursuit Cards.southronPursuit

-- | Each enemy in the staging area gets -10 engagement cost.
instance HasModifiersFor SouthronPursuit where
  getModifiersFor (SouthronPursuit a) = when a.flipped do
    staged <- select EnemyInStagingArea
    for_ staged \e -> modified a e [EngagementCostModifier (-10)]

instance HasAbilities SouthronPursuit where
  getAbilities (SouthronPursuit a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.flipped]

{- | When Revealed: Set each other stage 3A aside, out of play. Shuffle the encounter
discard pile and the Harad Soldiers encounter set into the encounter deck. Then, each
player reveals an encounter card.
Forced: After an enemy engages a player, remove 1 progress from this stage.
-}
instance RunMessage SouthronPursuit where
  runMessage msg q@(SouthronPursuit attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      SouthronPursuit <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      runStage3A HaradSoldiers
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      let engaged = length [() | Window _ (EnemyEngaged _ _) <- ws]
      when (attrs.progress > 0) $ removeTokens attrs attrs Progress (min attrs.progress engaged)
      pure q
    _ -> SouthronPursuit <$> runMessage msg attrs
