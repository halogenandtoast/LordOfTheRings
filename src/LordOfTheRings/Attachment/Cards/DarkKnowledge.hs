module LordOfTheRings.Attachment.Cards.DarkKnowledge (darkKnowledge) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DarkKnowledge = DarkKnowledge AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

darkKnowledge :: AttachmentCard DarkKnowledge
darkKnowledge = attachment DarkKnowledge Cards.darkKnowledge

instance HasModifiersFor DarkKnowledge where
  getModifiersFor (DarkKnowledge a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier (-1)]
    _ -> pure ()
