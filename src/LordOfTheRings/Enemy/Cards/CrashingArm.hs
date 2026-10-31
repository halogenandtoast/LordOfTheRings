module LordOfTheRings.Enemy.Cards.CrashingArm (crashingArm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype CrashingArm = CrashingArm EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crashingArm :: EnemyCard CrashingArm
crashingArm = enemy CrashingArm Cards.crashingArm

-- | Grapple (fewest enemies grappled).
-- Forced: After Crashing Arm is detached from a location, deal it 3 shadow cards. A
-- grappling enemy lets go for the staging area.
-- Shadow: Attacking enemy makes an additional attack after this one.
instance RunMessage CrashingArm where
  runMessage msg e@(CrashingArm attrs) = case msg of
    PlaceEnemy eid StagingArea | eid == attrs.id, AttachedTo (LocationTarget _) <- attrs.placement -> do
      replicateM_ 3 $ dealShadowCard attrs.id
      CrashingArm <$> runMessage msg attrs
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure e
    _ -> CrashingArm <$> runMessage msg attrs
