module LordOfTheRings.Attachment.Cards.BeyondAllHope (beyondAllHope) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BeyondAllHope = BeyondAllHope AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Treason of Saruman. Permanent, on its card def. Setup: Attach to a hero:
-- as the Campaign Pool records it on its hero.
beyondAllHope :: AttachmentCard BeyondAllHope
beyondAllHope = attachment BeyondAllHope Cards.beyondAllHope & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +1 [willpower], +1 [attack], +1 [defense] and cannot have treachery
-- card attachments.
instance HasModifiersFor BeyondAllHope where
  getModifiersFor (BeyondAllHope a) = withHost a \host ->
    modified a host [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1, CannotHaveTreacheryAttachments]
