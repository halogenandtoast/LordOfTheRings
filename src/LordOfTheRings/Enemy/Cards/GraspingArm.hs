module LordOfTheRings.Enemy.Cards.GraspingArm (graspingArm) where

import LordOfTheRings.Card (EncounterCard (MkEncounterCard), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GraspingArm = GraspingArm EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

graspingArm :: EnemyCard GraspingArm
graspingArm = enemy GraspingArm Cards.graspingArm

-- | Forced: When Grasping Arm attacks, the engaged player must exhaust a character he
-- controls.
instance HasAbilities GraspingArm where
  getAbilities (GraspingArm a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

-- | Shadow: If this attack is undefended, resolve Grasping Arm's Grapple keyword: the
-- shadow card leaves the attacking enemy to grapple a location.
instance RunMessage GraspingArm where
  runMessage msg e@(GraspingArm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> chooseAndExhaust attrs pid 1
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      when (null defenders) $ grappleShadowCard eid (MkEncounterCard cid (toCardCode attrs))
      pure e
    _ -> GraspingArm <$> runMessage msg attrs
