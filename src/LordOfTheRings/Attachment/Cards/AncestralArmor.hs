module LordOfTheRings.Attachment.Cards.AncestralArmor (ancestralArmor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Trait (Trait (Noble))

newtype AncestralArmor = AncestralArmor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a [leadership] or Noble character. Restricted. Limit 1 per character, on its
-- card def.
ancestralArmor :: AttachmentCard AncestralArmor
ancestralArmor =
  attachment AncestralArmor Cards.ancestralArmor
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithSphere Leadership, HeroWithTrait Noble])
          (AllyMatchAny [AllyWithSphere Leadership, AllyWithTrait Noble])
      )

-- | Attached character gets +2 [defense] and +2 hit points.
instance HasModifiersFor AncestralArmor where
  getModifiersFor (AncestralArmor a) = withHost a \host -> modified a host [DefenseModifier 2, HitPointModifier 2]
