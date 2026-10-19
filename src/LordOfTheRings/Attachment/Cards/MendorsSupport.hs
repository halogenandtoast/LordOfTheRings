module LordOfTheRings.Attachment.Cards.MendorsSupport (mendorsSupport) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MendorsSupport = MendorsSupport AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

mendorsSupport :: AttachmentCard MendorsSupport
mendorsSupport = attachment MendorsSupport Cards.mendorsSupport
