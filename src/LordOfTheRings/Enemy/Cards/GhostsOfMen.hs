module LordOfTheRings.Enemy.Cards.GhostsOfMen (ghostsOfMen) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOr)

newtype GhostsOfMen = GhostsOfMen EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Phantom.
ghostsOfMen :: EnemyCard GhostsOfMen
ghostsOfMen = enemy GhostsOfMen Cards.ghostsOfMen

-- | When Revealed: Either attach a set aside Overcome by Fear to your threat dial, or Ghosts
-- of Men makes an immediate attack against you.
instance RunMessage GhostsOfMen where
  runMessage msg e@(GhostsOfMen attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      attachOvercomeByFearOr attrs revealer "ghostsOfMen" True $ immediateAttack revealer attrs.id
      pure e
    _ -> GhostsOfMen <$> runMessage msg attrs
