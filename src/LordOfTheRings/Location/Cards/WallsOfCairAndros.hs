module LordOfTheRings.Location.Cards.WallsOfCairAndros (wallsOfCairAndros) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros.Helpers (runNightmareBattleground)

newtype WallsOfCairAndros = WallsOfCairAndros LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wallsOfCairAndros :: LocationCard WallsOfCairAndros
wallsOfCairAndros = location WallsOfCairAndros Cards.wallsOfCairAndros

-- | If Walls of Cair Andros has 4 or more damage, discard it and each player must assign 4
-- damage among characters he controls.
-- Shadow: Attacking enemy gets +X attack. X is the amount of damage on the active location.
instance RunMessage WallsOfCairAndros where
  runMessage msg l@(WallsOfCairAndros a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      damage <- sum <$> (traverse (getDamage . toTarget) =<< select IsActiveLocation)
      when (damage > 0) $ enemyGetsAttack a eid damage
      pure l
    _ -> WallsOfCairAndros <$> runNightmareBattleground (eachPlayer \pid -> dealDamageDivided a pid 4) msg a
