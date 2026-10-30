module LordOfTheRings.Enemy.Cards.SiegeShip (siegeShip) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (notGuarding, unguardedObjectives)

newtype SiegeShip = SiegeShip EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

siegeShip :: EnemyCard SiegeShip
siegeShip = enemy SiegeShip Cards.siegeShip

-- | Archery 2. Cannot have attachments.
instance HasModifiersFor SiegeShip where
  getModifiersFor (SiegeShip a) = modified a a [CannotHaveAttachments]

-- | While Siege Ship is in the staging area, it gains: "Forced: At the end of the quest
-- phase, if Siege Ship is not guarding an objective and there is an unguarded objective
-- in the staging area, attach Siege Ship to that objective, guarding it."
instance HasAbilities SiegeShip where
  getAbilities (SiegeShip a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When QuestPhase | a.placement == StagingArea]

instance RunMessage SiegeShip where
  runMessage msg e@(SiegeShip attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      free <- notNull <$> notGuarding [attrs.id]
      objectives <- unguardedObjectives
      lead <- getLead
      when free $ chooseOrRunOneM lead do
        questionLabeled "siegeShip.guard"
        causedBy attrs
        targets objectives \o -> guardObjectiveWith o attrs.cardId
      pure e
    _ -> SiegeShip <$> runMessage msg attrs
