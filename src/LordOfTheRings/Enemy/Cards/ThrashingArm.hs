module LordOfTheRings.Enemy.Cards.ThrashingArm (thrashingArm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype ThrashingArm = ThrashingArm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

thrashingArm :: EnemyCard ThrashingArm
thrashingArm = enemy ThrashingArm Cards.thrashingArm

-- | Forced: At the end of the combat phase, if Thrashing Arm is engaged with you, deal 3
-- damage to a character you control.
instance HasAbilities ThrashingArm where
  getAbilities (ThrashingArm a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When CombatPhase | EngagedWith _ <- [a.placement]]

instance RunMessage ThrashingArm where
  runMessage msg e@(ThrashingArm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> dealDamageToOneCharacter attrs pid 3
      pure e
    _ -> ThrashingArm <$> runMessage msg attrs
