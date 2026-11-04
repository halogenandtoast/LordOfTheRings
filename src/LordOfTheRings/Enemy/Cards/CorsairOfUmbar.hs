module LordOfTheRings.Enemy.Cards.CorsairOfUmbar (corsairOfUmbar) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (shipIsActive)

newtype CorsairOfUmbar = CorsairOfUmbar EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairOfUmbar :: EnemyCard CorsairOfUmbar
corsairOfUmbar = enemy CorsairOfUmbar Cards.corsairOfUmbar

-- | If the active location is a Ship, Corsair of Umbar gets +2 attack and +2 defense.
instance HasModifiersFor CorsairOfUmbar where
  getModifiersFor (CorsairOfUmbar a) = do
    ship <- shipIsActive
    when ship $ modifySelf a [AttackModifier 2, DefenseModifier 2]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the active location is a
-- Ship).
instance RunMessage CorsairOfUmbar where
  runMessage msg e@(CorsairOfUmbar attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ship <- shipIsActive
      enemyGetsAttack attrs eid (if ship then 2 else 1)
      pure e
    _ -> CorsairOfUmbar <$> runMessage msg attrs
