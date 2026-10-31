module LordOfTheRings.Location.Cards.WarTornWaters (warTornWaters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (guardingCards, unguardedObjectives)

newtype WarTornWaters = WarTornWaters LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

warTornWaters :: LocationCard WarTornWaters
warTornWaters = location WarTornWaters Cards.warTornWaters

-- | While War-torn Waters is in the staging area, it gains: "Forced: At the end of the
-- quest phase, if War-torn Waters is not guarding an objective and there is an unguarded
-- objective in the staging area, attach War-torn Waters to that objective, guarding it."
instance HasAbilities WarTornWaters where
  getAbilities (WarTornWaters a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == StagingArea]

-- | Travel: Deal 2 damage to each Ship objective to travel here.
instance RunMessage WarTornWaters where
  runMessage msg (WarTornWaters a) = case msg of
    TravelTo lid | lid == a.id -> do
      ships <- select shipObjective
      for_ ships \s -> dealDamage a s 2
      WarTornWaters <$> runMessage msg a
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      guarding <- elem a.cardId <$> guardingCards
      objectives <- unguardedObjectives
      lead <- getLead
      unless guarding $ chooseOrRunOneM lead do
        questionLabeled "warTornWaters.guard"
        causedBy a
        targets objectives \o -> guardObjectiveWith o a.cardId
      pure $ WarTornWaters a
    _ -> WarTornWaters <$> runMessage msg a
