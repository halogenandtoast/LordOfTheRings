module LordOfTheRings.Enemy.Cards.SahirsPursuer (sahirsPursuer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (StagingArea))

newtype SahirsPursuer = SahirsPursuer EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

sahirsPursuer :: EnemyCard SahirsPursuer
sahirsPursuer = enemy SahirsPursuer Cards.sahirsPursuer

-- | Boarding 1. Cannot have attachments.
instance HasModifiersFor SahirsPursuer where
  getModifiersFor (SahirsPursuer a) = modifySelf a [CannotHaveAttachments]

-- | Forced: At the end of the encounter phase, if Sahír's Pursuer is in the staging area,
-- the first player must either engage it or shift your heading off-course.
instance HasAbilities SahirsPursuer where
  getAbilities (SahirsPursuer a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When EncounterPhase | a.placement == StagingArea]

instance RunMessage SahirsPursuer where
  runMessage msg e@(SahirsPursuer attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      chooseOneM lead do
        questionLabeled "sahirsPursuer.choose"
        causedBy attrs
        labeled "sahirsPursuer.engage" $ engageEnemy lead attrs.id
        labeled "shiftHeadingOffCourse" $ shiftHeadingOffCourse attrs
      pure e
    _ -> SahirsPursuer <$> runMessage msg attrs
