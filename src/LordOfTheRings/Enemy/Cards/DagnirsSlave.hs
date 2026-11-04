module LordOfTheRings.Enemy.Cards.DagnirsSlave (dagnirsSlave) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.MountGundabad.Helpers

newtype DagnirsSlave = DagnirsSlave EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

dagnirsSlave :: EnemyCard DagnirsSlave
dagnirsSlave = enemy DagnirsSlave Cards.dagnirsSlave

-- | Dagnir cannot take damage from non-quest effects.
instance HasModifiersFor DagnirsSlave where
  getModifiersFor (DagnirsSlave a) = getDagnir >>= traverse_ \d -> modified a d [CannotTakeNonQuestDamage]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if Dagnir is engaged with
-- you).
instance RunMessage DagnirsSlave where
  runMessage msg e@(DagnirsSlave attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      engaged <- maybe (pure False) (\d -> selectAny (EnemyWithId d <> EnemyEngagedWith (PlayerWithId pid))) =<< getDagnir
      enemyGetsAttack attrs eid (if engaged then 2 else 1)
      pure e
    _ -> DagnirsSlave <$> runMessage msg attrs
