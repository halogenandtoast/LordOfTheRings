module LordOfTheRings.Attachment.Cards.StewardOfGondor (stewardOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype StewardOfGondor = StewardOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

stewardOfGondor :: AttachmentCard StewardOfGondor
stewardOfGondor = attachment StewardOfGondor Cards.stewardOfGondor
