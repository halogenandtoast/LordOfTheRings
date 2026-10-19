module LordOfTheRings.Attachment.Cards.DunedainMark (dunedainMark) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DunedainMark = DunedainMark AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainMark :: AttachmentCard DunedainMark
dunedainMark = attachment DunedainMark Cards.dunedainMark
