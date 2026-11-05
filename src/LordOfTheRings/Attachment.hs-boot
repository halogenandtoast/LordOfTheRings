-- | The attachment registry, for "LordOfTheRings.Helpers.Attachment": an attachment card's
-- "Attach to" is read from its builder, and an attachment card may ask where another can go,
-- as Anvil of Gundabad plays an Item attachment from hand.
module LordOfTheRings.Attachment (lookupAttachment) where

import GHC.Stack (HasCallStack)
import LordOfTheRings.Attachment.Types (Attachment)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Id (AttachmentId, CardId, PlayerId)

lookupAttachment :: (HasCallStack) => CardCode -> CardId -> AttachmentId -> PlayerId -> Attachment
