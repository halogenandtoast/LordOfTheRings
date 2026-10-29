module LordOfTheRings.Enemy.Cards.DeadlyHuorn (deadlyHuorn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype DeadlyHuorn = DeadlyHuorn EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadlyHuorn :: EnemyCard DeadlyHuorn
deadlyHuorn = enemy DeadlyHuorn Cards.deadlyHuorn

-- | Cannot have attachments.
instance HasModifiersFor DeadlyHuorn where
  getModifiersFor (DeadlyHuorn a) = modified a a [CannotHaveAttachments]

-- | Forced: At the beginning of each resource phase, the engaged player must deal 3 damage
-- to a character he controls.
instance HasAbilities DeadlyHuorn where
  getAbilities (DeadlyHuorn a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After ResourcePhase | EngagedWith _ <- [a.placement]]

instance RunMessage DeadlyHuorn where
  runMessage msg e@(DeadlyHuorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      dealDamageToOneCharacter attrs pid 3
      pure e
    _ -> DeadlyHuorn <$> runMessage msg attrs
