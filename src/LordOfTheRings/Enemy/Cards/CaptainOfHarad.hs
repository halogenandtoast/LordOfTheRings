module LordOfTheRings.Enemy.Cards.CaptainOfHarad (captainOfHarad) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (haradEnemy)

newtype CaptainOfHarad = CaptainOfHarad EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

captainOfHarad :: EnemyCard CaptainOfHarad
captainOfHarad = enemy CaptainOfHarad Cards.captainOfHarad

-- | Forced: After Captain of Harad engages you, each Harad enemy gets +1 attack and -10
-- engagement cost until the end of the round.
instance HasAbilities CaptainOfHarad where
  getAbilities (CaptainOfHarad a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: If this attack destroys a character, return attacking enemy to the staging
-- area.
instance RunMessage CaptainOfHarad where
  runMessage msg e@(CaptainOfHarad attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      harad <- select haradEnemy
      for_ harad \h -> roundModifiers attrs h [AttackModifier 1, EngagementCostModifier (-10)]
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == attrs.cardId -> do
      placeEnemy eid StagingArea
      pure e
    _ -> CaptainOfHarad <$> runMessage msg attrs
