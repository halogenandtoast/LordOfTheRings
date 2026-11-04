module LordOfTheRings.Attachment.Cards.SilverCirclet (silverCirclet) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype SilverCirclet = SilverCirclet AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Spirit hero. Limit 1 per hero. Restricted, on its card def.
silverCirclet :: AttachmentCard SilverCirclet
silverCirclet = attachment SilverCirclet Cards.silverCirclet & attachTo (AttachToHero $ HeroWithSphere Spirit)

-- | Attached hero gets +2 willpower.
instance HasModifiersFor SilverCirclet where
  getModifiersFor (SilverCirclet a) = withHost a \host -> modified a host [WillpowerModifier 2]
