module LordOfTheRings.Attachment.Cards.CitadelPlate (citadelPlate) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype CitadelPlate = CitadelPlate AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

citadelPlate :: AttachmentCard CitadelPlate
citadelPlate = attachment CitadelPlate Cards.citadelPlate
