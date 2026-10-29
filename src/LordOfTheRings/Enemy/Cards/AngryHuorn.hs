module LordOfTheRings.Enemy.Cards.AngryHuorn (angryHuorn) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (ResourcePhase))
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype AngryHuorn = AngryHuorn EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

angryHuorn :: EnemyCard AngryHuorn
angryHuorn = enemy AngryHuorn Cards.angryHuorn

-- | Cannot have attachments.
instance HasModifiersFor AngryHuorn where
  getModifiersFor (AngryHuorn a) = modified a a [CannotHaveAttachments]

-- | Forced: At the beginning of each resource phase, Angry Huorn attacks the engaged player.
instance HasAbilities AngryHuorn where
  getAbilities (AngryHuorn a) =
    [mkAbility a 1 $ forced $ PhaseBeginsWindow After ResourcePhase | EngagedWith _ <- [a.placement]]

instance RunMessage AngryHuorn where
  runMessage msg e@(AngryHuorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ | EngagedWith pid <- attrs.placement -> do
      immediateAttack pid attrs.id
      pure e
    _ -> AngryHuorn <$> runMessage msg attrs
