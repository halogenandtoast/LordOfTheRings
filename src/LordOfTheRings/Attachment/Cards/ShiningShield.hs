module LordOfTheRings.Attachment.Cards.ShiningShield (shiningShield) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noble))

newtype ShiningShield = ShiningShield AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noble hero. Limit 1 per hero and restricted, on its card def.
shiningShield :: AttachmentCard ShiningShield
shiningShield = attachment ShiningShield Cards.shiningShield & attachTo (AttachToHero $ HeroWithTrait Noble)

-- | Attached hero gets +1 defense (+2 defense instead if your threat is 40 or higher).
instance HasModifiersFor ShiningShield where
  getModifiersFor (ShiningShield a) = withHost a \host -> do
    threat <- getThreat a.controller
    modified a host [DefenseModifier (if threat >= 40 then 2 else 1)]
