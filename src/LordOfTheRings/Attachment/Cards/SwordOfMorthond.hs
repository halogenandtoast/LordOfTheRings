module LordOfTheRings.Attachment.Cards.SwordOfMorthond (swordOfMorthond) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor, Outlands))

newtype SwordOfMorthond = SwordOfMorthond AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Gondor ally.
swordOfMorthond :: AttachmentCard SwordOfMorthond
swordOfMorthond = attachment SwordOfMorthond Cards.swordOfMorthond & attachTo (AttachToAlly $ AllyWithTrait Gondor)

-- | Attached ally gains the Outlands trait.
instance HasModifiersFor SwordOfMorthond where
  getModifiersFor (SwordOfMorthond a) = withHost a \host -> modified a host [GainTrait Outlands]
