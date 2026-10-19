module LordOfTheRings.Attachment.Cards.UnexpectedCourage (unexpectedCourage) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype UnexpectedCourage = UnexpectedCourage AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

unexpectedCourage :: AttachmentCard UnexpectedCourage
unexpectedCourage = attachment UnexpectedCourage Cards.unexpectedCourage
