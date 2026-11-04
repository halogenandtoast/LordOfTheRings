module LordOfTheRings.Attachment.Cards.MirkwoodLongKnife (mirkwoodLongKnife) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype MirkwoodLongKnife = MirkwoodLongKnife AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Silvan hero. Restricted, on its card def.
mirkwoodLongKnife :: AttachmentCard MirkwoodLongKnife
mirkwoodLongKnife = attachment MirkwoodLongKnife Cards.mirkwoodLongKnife & attachTo (AttachToHero $ HeroWithTrait Silvan)

-- | Attached hero gets +1 willpower and +1 attack.
instance HasModifiersFor MirkwoodLongKnife where
  getModifiersFor (MirkwoodLongKnife a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1, AttackModifier 1]
    _ -> pure ()
