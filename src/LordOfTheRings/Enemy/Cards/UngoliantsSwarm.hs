module LordOfTheRings.Enemy.Cards.UngoliantsSwarm (ungoliantsSwarm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UngoliantsSwarm = UngoliantsSwarm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ungoliantsSwarm :: EnemyCard UngoliantsSwarm
ungoliantsSwarm = enemy UngoliantsSwarm Cards.ungoliantsSwarm

-- | Forced: After Ungoliant's Swarm engages a player, that player must choose and exhaust
-- 2 characters they control.
-- Shadow: Put Ungoliant's Swarm into play engaged with you.
instance HasAbilities UngoliantsSwarm where
  getAbilities (UngoliantsSwarm a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage UngoliantsSwarm where
  runMessage msg e@(UngoliantsSwarm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> chooseAndExhaust attrs pid 2
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      putShadowEnemyIntoPlay eid cid pid
      pure e
    _ -> UngoliantsSwarm <$> runMessage msg attrs
