module LordOfTheRings.Treachery.Cards.GuffawingOfGiants (guffawingOfGiants) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim.Helpers (stoneGiant)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype GuffawingOfGiants = GuffawingOfGiants TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

guffawingOfGiants :: TreacheryCard GuffawingOfGiants
guffawingOfGiants = treachery GuffawingOfGiants Cards.guffawingOfGiants

{- | When Revealed: The first player chooses 1 Stone-giant in the staging area. At the end
of the quest phase, that Stone-giant engages the player with the highest threat. If there
are no Stone-giant cards in the staging area, search the encounter deck for 1 Stone-giant
and add it to the staging area.

The chosen Stone-giant carries the effect until the end of the quest phase, this one or
the next.
-}
instance RunMessage GuffawingOfGiants where
  runMessage msg t@(GuffawingOfGiants attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      giants <- select (stoneGiant <> EnemyInStagingArea)
      lead <- getLead
      if null giants
        then searchEncounterDeckToStaging lead (cardIs Enemies.stoneGiant)
        else chooseOrRunOneM lead do
          questionLabeled "guffawingOfGiants.choose"
          causedBy attrs
          targets giants \g -> createEffect attrs g [EngagesHighestThreatAtEndOfQuestPhase] (UntilEndOfPhaseOf QuestPhase)
      pure t
    _ -> GuffawingOfGiants <$> runMessage msg attrs
