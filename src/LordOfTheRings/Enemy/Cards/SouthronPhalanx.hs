module LordOfTheRings.Enemy.Cards.SouthronPhalanx (southronPhalanx) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype SouthronPhalanx = SouthronPhalanx EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronPhalanx :: EnemyCard SouthronPhalanx
southronPhalanx = enemy SouthronPhalanx Cards.southronPhalanx

-- | While engaged with a player, Southron Phalanx gets +1 attack for each location that
-- player controls.
instance HasModifiersFor SouthronPhalanx where
  getModifiersFor (SouthronPhalanx a) = case a.placement of
    EngagedWith pid -> do
      n <- length <$> controlledLocations pid
      when (n > 0) $ modified a a [AttackModifier n]
    _ -> pure ()

-- | Shadow: Attacking enemy gets +1 attack for each location the defending player
-- controls.
instance RunMessage SouthronPhalanx where
  runMessage msg e@(SouthronPhalanx attrs) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      n <- length <$> controlledLocations pid
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure e
    _ -> SouthronPhalanx <$> runMessage msg attrs
