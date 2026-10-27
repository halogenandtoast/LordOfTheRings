module LordOfTheRings.Enemy.Cards.GoblinScout (goblinScout) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinScout = GoblinScout EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinScout :: EnemyCard GoblinScout
goblinScout = enemy GoblinScout Cards.goblinScout

-- | Each player with a threat of 25 or higher cannot optionally engage Goblin Scout.
instance HasModifiersFor GoblinScout where
  getModifiersFor (GoblinScout a) = modified a a [CannotBeOptionallyEngagedBy (PlayerWithThreatAtLeast 25)]
