module LordOfTheRings.Enemy.Cards.GoblinArcher (goblinArcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinArcher = GoblinArcher EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinArcher :: EnemyCard GoblinArcher
goblinArcher = enemy GoblinArcher Cards.goblinArcher

-- | Players cannot optionally engage Goblin Archer. Characters with ranged are eligible to
-- attack Goblin Archer while it is in the staging area.
instance HasModifiersFor GoblinArcher where
  getModifiersFor (GoblinArcher a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer, AttackableInStagingAreaByRanged]
