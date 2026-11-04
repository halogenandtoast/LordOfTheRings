module LordOfTheRings.Quest.Cards.OrcPatrols (orcPatrols) where

import LordOfTheRings.EncounterSet (EncounterSet (MordorOrcs))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (runStage3A)

newtype OrcPatrols = OrcPatrols QuestAttrs
  deriving anyclass IsQuest
  deriving newtype (Show, Eq, ToJSON, Entity)

orcPatrols :: QuestCard OrcPatrols
orcPatrols = quest OrcPatrols Cards.orcPatrols

-- | Orc Patrols gets +2 quest points for each enemy in the staging area.
instance HasModifiersFor OrcPatrols where
  getModifiersFor (OrcPatrols a) = when a.flipped do
    staged <- length <$> select EnemyInStagingArea
    when (staged > 0) $ modifySelf a [QuestPointModifier (2 * staged)]

instance HasAbilities OrcPatrols where
  getAbilities (OrcPatrols a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.flipped]

{- | When Revealed: Set each other stage 3A aside, out of play. Shuffle the encounter
discard pile and the Mordor Orcs encounter set into the encounter deck. Then, each player
reveals an encounter card.
Forced: After an enemy engages a player, raise that player's threat by 1.
-}
instance RunMessage OrcPatrols where
  runMessage msg q@(OrcPatrols attrs) = case msg of
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      OrcPatrols <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      runStage3A MordorOrcs
      pure q
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> encounterRaisesThreat pid 1
      pure q
    _ -> OrcPatrols <$> runMessage msg attrs
