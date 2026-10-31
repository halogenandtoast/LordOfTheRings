module LordOfTheRings.Enemy.Cards.DruadanWarrior (druadanWarrior) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DruadanWarrior = DruadanWarrior EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

druadanWarrior :: EnemyCard DruadanWarrior
druadanWarrior = enemy DruadanWarrior Cards.druadanWarrior

-- | Prowl 2.
-- Each hero with no resources in its resource pool cannot be declared as an attacker or
-- defender against Drúadan Warrior.
instance HasModifiersFor DruadanWarrior where
  getModifiersFor (DruadanWarrior a) = modified a a [NotFoughtByHeroesWithoutResources]

instance RunMessage DruadanWarrior where
  runMessage msg (DruadanWarrior attrs) = DruadanWarrior <$> runMessage msg attrs
