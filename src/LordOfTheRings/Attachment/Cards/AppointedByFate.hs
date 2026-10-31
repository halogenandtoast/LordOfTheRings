module LordOfTheRings.Attachment.Cards.AppointedByFate (appointedByFate) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype AppointedByFate = AppointedByFate AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Revised Core campaign, attached as Escape from Dol Guldur is set up.
appointedByFate :: AttachmentCard AppointedByFate
appointedByFate = attachment AppointedByFate Cards.appointedByFate

-- | Attached hero collects 1 additional resource during each resource phase.
instance HasModifiersFor AppointedByFate where
  getModifiersFor (AppointedByFate a) = withHost a \host -> modified a host [CollectsAdditionalResources 1]
