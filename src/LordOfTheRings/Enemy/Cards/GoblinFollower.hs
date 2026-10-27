module LordOfTheRings.Enemy.Cards.GoblinFollower (goblinFollower) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinFollower = GoblinFollower EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinFollower :: EnemyCard GoblinFollower
goblinFollower = enemy GoblinFollower Cards.goblinFollower

-- | When Revealed: Goblin Follower engages the last player.
-- Shadow: attacking enemy gets +1 [Attack] (+2 [Attack] instead if attacking the last
-- player.)
instance RunMessage GoblinFollower where
  runMessage msg e@(GoblinFollower attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lastPlayer <- getLastPlayer
      engageEnemy lastPlayer attrs.id
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      lastPlayer <- getLastPlayer
      enemyGetsAttack attrs eid (if pid == lastPlayer then 2 else 1)
      pure e
    _ -> GoblinFollower <$> runMessage msg attrs
