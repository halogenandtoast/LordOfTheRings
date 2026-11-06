module LordOfTheRings.Enemy.Cards.GraspingRoot (graspingRoot) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheOldForest.Helpers (victoryLocations)

newtype GraspingRoot = GraspingRoot EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

graspingRoot :: EnemyCard GraspingRoot
graspingRoot = enemy GraspingRoot Cards.graspingRoot

instance HasAbilities GraspingRoot where
  getAbilities (GraspingRoot a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage GraspingRoot where
  runMessage msg e@(GraspingRoot a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      n <- length <$> victoryLocations
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        characters <- charactersControlledBy pid
        dealDamageDividedAmong a pid characters n
      pure e
    _ -> GraspingRoot <$> runMessage msg a
