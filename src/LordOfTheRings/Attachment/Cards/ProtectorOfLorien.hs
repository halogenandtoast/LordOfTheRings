module LordOfTheRings.Attachment.Cards.ProtectorOfLorien (protectorOfLorien) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ProtectorOfLorien = ProtectorOfLorien AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

protectorOfLorien :: AttachmentCard ProtectorOfLorien
protectorOfLorien = attachment ProtectorOfLorien Cards.protectorOfLorien
