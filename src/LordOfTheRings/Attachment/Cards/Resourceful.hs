module LordOfTheRings.Attachment.Cards.Resourceful (resourceful) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Resourceful = Resourceful AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Secrecy 3, on its card def. Attach to a hero you control.
resourceful :: AttachmentCard Resourceful
resourceful =
  attachment Resourceful Cards.resourceful
    & fmap (overAttrs \a -> a {attachmentAttachTo = AttachToHero (HeroControlledBy $ PlayerWithId a.owner)})

-- | Attached hero collects 1 additional resource during the resource phase each round.
instance HasModifiersFor Resourceful where
  getModifiersFor (Resourceful a) = withHost a \host -> modified a host [CollectsAdditionalResources 1]
