module LordOfTheRings.Enemy.Cards.SouthronCaptain (southronCaptain) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Trait (Trait (Harad))

newtype SouthronCaptain = SouthronCaptain EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronCaptain :: EnemyCard SouthronCaptain
southronCaptain = enemy SouthronCaptain Cards.southronCaptain

-- | While Southron Captain is in the staging area, each other Harad enemy gets +1 threat.
-- While Southron Captain is engaged with you, each other Harad enemy engaged with you gets
-- +1 attack.
instance HasModifiersFor SouthronCaptain where
  getModifiersFor (SouthronCaptain a) = case a.placement of
    StagingArea -> do
      others <- select (EnemyWithTrait Harad <> not_ (EnemyWithId a.id))
      for_ others \e -> modified a e [ThreatModifier 1]
    EngagedWith pid -> do
      others <- select (EnemyWithTrait Harad <> EnemyEngagedWith (PlayerWithId pid) <> not_ (EnemyWithId a.id))
      for_ others \e -> modified a e [AttackModifier 1]
    _ -> pure ()

instance RunMessage SouthronCaptain where
  runMessage msg (SouthronCaptain attrs) = SouthronCaptain <$> runMessage msg attrs
