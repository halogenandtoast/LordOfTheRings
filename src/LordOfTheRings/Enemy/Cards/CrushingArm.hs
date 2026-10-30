module LordOfTheRings.Enemy.Cards.CrushingArm (crushingArm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo, StagingArea))

newtype CrushingArm = CrushingArm EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crushingArm :: EnemyCard CrushingArm
crushingArm = enemy CrushingArm Cards.crushingArm

-- | Forced: After Crushing Arm is detached from a location, it makes an immediate attack
-- against the first player: as it lets go of the location for the staging area.
instance RunMessage CrushingArm where
  runMessage msg (CrushingArm attrs) = case msg of
    PlaceEnemy eid StagingArea | eid == attrs.id, AttachedTo (LocationTarget _) <- attrs.placement -> do
      lead <- getLead
      immediateAttack lead attrs.id
      CrushingArm <$> runMessage msg attrs
    _ -> CrushingArm <$> runMessage msg attrs
