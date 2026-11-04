module LordOfTheRings.Enemy.Cards.TheTwoWatchersEnemy (theTwoWatchersEnemy) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype TheTwoWatchersEnemy = TheTwoWatchersEnemy EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The enemy side of The Two Watchers, its location side turned over. The stages make it
-- engaged with each player while it is in the staging area.
theTwoWatchersEnemy :: EnemyCard TheTwoWatchersEnemy
theTwoWatchersEnemy = enemy TheTwoWatchersEnemy Cards.theTwoWatchersEnemy

{- | Immune to player card effects. Cannot be optionally engaged.
Forced: When The Two Watchers deals any amount of damage, cancel that damage and raise the
defending player's threat by X instead. X is the amount of damage just canceled.
-}
instance HasModifiersFor TheTwoWatchersEnemy where
  getModifiersFor (TheTwoWatchersEnemy a) =
    modifySelf a [ImmuneToPlayerCardEffects, CannotBeOptionallyEngagedBy AnyPlayer, AttackDamageToThreatInstead]

instance RunMessage TheTwoWatchersEnemy where
  runMessage msg (TheTwoWatchersEnemy attrs) = TheTwoWatchersEnemy <$> runMessage msg attrs
