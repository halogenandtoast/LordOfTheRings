module LordOfTheRings.Enemy.Cards.CorsairCaptain (corsairCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (shipIsActive)
import LordOfTheRings.Trait (Trait (Ship))

newtype CorsairCaptain = CorsairCaptain EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

corsairCaptain :: EnemyCard CorsairCaptain
corsairCaptain = enemy CorsairCaptain Cards.corsairCaptain

-- | Cannot be optionally engaged. If the active location is a Ship, Corsair Captain gets
-- -30 engagement cost.
instance HasModifiersFor CorsairCaptain where
  getModifiersFor (CorsairCaptain a) = do
    ship <- shipIsActive
    modifySelf a $ CannotBeOptionallyEngagedBy AnyPlayer : [EngagementCostModifier (-30) | ship]

-- | If the active location is a Ship, it gains: "Forced: When Corsair Captain engages you,
-- it makes an immediate attack."
instance HasAbilities CorsairCaptain where
  getAbilities (CorsairCaptain a) =
    [restricted a 1 (exists $ IsActiveLocation <> LocationWithTrait Ship) $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage CorsairCaptain where
  runMessage msg e@(CorsairCaptain attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    _ -> CorsairCaptain <$> runMessage msg attrs
