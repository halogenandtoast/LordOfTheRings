module LordOfTheRings.Enemy.Cards.BrashCorsair (brashCorsair) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype BrashCorsair = BrashCorsair EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brashCorsair :: EnemyCard BrashCorsair
brashCorsair = enemy BrashCorsair Cards.brashCorsair

-- | Forced: When the engaged player exhausts a Ship objective, Brash Corsair makes an
-- immediate attack.
instance RunMessage BrashCorsair where
  runMessage msg (BrashCorsair attrs) = case msg of
    CheckWindows ws | EngagedWith pid <- attrs.placement -> do
      ships <- map toTarget <$> shipObjectivesOf pid
      when (notNull [() | Window After (Exhausted t) <- ws, t `elem` ships]) $ immediateAttack pid attrs.id
      BrashCorsair <$> runMessage msg attrs
    _ -> BrashCorsair <$> runMessage msg attrs
