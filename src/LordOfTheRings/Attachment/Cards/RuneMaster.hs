module LordOfTheRings.Attachment.Cards.RuneMaster (runeMaster) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Trait (Trait (Ranger, Signal))

newtype RuneMaster = RuneMaster AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Ranger or leadership hero.
runeMaster :: AttachmentCard RuneMaster
runeMaster =
  attachment RuneMaster Cards.runeMaster
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Ranger, HeroWithSphere Leadership])

-- | Response: After a Signal attachment is attached to a hero, exhaust Rune-master to add
-- 1 resource to that hero's resource pool.
instance HasAbilities RuneMaster where
  getAbilities (RuneMaster a) = [mkAbility a 1 $ ResponseAbility (TraitAttachedToHeroWindow After Signal) (exhaustCost a)]

instance RunMessage RuneMaster where
  runMessage msg s@(RuneMaster a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [h | Window _ (Attached _ h@(HeroTarget _) _) <- windows]) \h -> placeResources a h 1
      pure s
    _ -> RuneMaster <$> runMessage msg a
