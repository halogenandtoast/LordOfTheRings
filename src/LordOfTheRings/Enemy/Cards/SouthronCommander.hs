module LordOfTheRings.Enemy.Cards.SouthronCommander (southronCommander) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers

newtype SouthronCommander = SouthronCommander EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronCommander :: EnemyCard SouthronCommander
southronCommander = enemy SouthronCommander Cards.southronCommander

-- | When Revealed: Southron Commander makes an immediate attack from the staging area
-- against each player who controls at least 1 location.
instance RunMessage SouthronCommander where
  runMessage msg e@(SouthronCommander attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        holding <- notNull <$> controlledLocations pid
        when holding $ immediateAttack pid attrs.id
      pure e
    _ -> SouthronCommander <$> runMessage msg attrs
