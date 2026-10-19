module LordOfTheRings.Attachment.Cards.HornOfGondor (hornOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype HornOfGondor = HornOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

hornOfGondor :: AttachmentCard HornOfGondor
hornOfGondor = attachment HornOfGondor Cards.hornOfGondor
