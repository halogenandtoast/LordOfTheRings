module LordOfTheRings.Enemy.Cards.GiantBeetle (giantBeetle) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers (isSafe)

newtype GiantBeetle = GiantBeetle EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

giantBeetle :: EnemyCard GiantBeetle
giantBeetle = enemy GiantBeetle Cards.giantBeetle

-- | Damaged characters cannot be declared as attackers or defenders against Giant Beetle.
instance HasModifiersFor GiantBeetle where
  getModifiersFor (GiantBeetle a) = modified a a [NotFoughtByDamaged]

-- | While Giant Beetle is engaged with a player, it gains: "As an additional cost to travel
-- to a safe location, Giant Beetle makes an immediate attack against the engaged player."
-- Paid as a Travel cost is: the attack resolves before the location becomes active.
instance RunMessage GiantBeetle where
  runMessage msg (GiantBeetle attrs) = case msg of
    TravelTo lid | EngagedWith pid <- attrs.placement -> do
      whenM (isSafe lid) $ immediateAttack pid attrs.id
      GiantBeetle <$> runMessage msg attrs
    _ -> GiantBeetle <$> runMessage msg attrs
