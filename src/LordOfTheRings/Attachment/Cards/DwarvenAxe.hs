module LordOfTheRings.Attachment.Cards.DwarvenAxe (dwarvenAxe) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DwarvenAxe = DwarvenAxe AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwarvenAxe :: AttachmentCard DwarvenAxe
dwarvenAxe = attachment DwarvenAxe Cards.dwarvenAxe
