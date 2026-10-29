module LordOfTheRings.Enemy.Cards.DarkHeartedHuorn (darkHeartedHuorn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype DarkHeartedHuorn = DarkHeartedHuorn EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkHeartedHuorn :: EnemyCard DarkHeartedHuorn
darkHeartedHuorn = enemy DarkHeartedHuorn Cards.darkHeartedHuorn

-- | Cannot have attachments.
instance HasModifiersFor DarkHeartedHuorn where
  getModifiersFor (DarkHeartedHuorn a) = modified a a [CannotHaveAttachments]

-- | Forced: At the beginning of each resource phase, the engaged player must raise his
-- threat by 2.
instance HasAbilities DarkHeartedHuorn where
  getAbilities (DarkHeartedHuorn a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After ResourcePhase | EngagedWith _ <- [a.placement]]

instance RunMessage DarkHeartedHuorn where
  runMessage msg e@(DarkHeartedHuorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      encounterRaisesThreat pid 2
      pure e
    _ -> DarkHeartedHuorn <$> runMessage msg attrs
