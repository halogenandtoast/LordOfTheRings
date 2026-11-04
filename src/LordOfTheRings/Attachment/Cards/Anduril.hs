module LordOfTheRings.Attachment.Cards.Anduril (anduril) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Anduril = Anduril AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noble hero or Aragorn. Restricted. Its card def only, for The Battle of the Pelennor Fields to attach: the player
-- cards' own version gives its text.
anduril :: AttachmentCard Anduril
anduril = attachment Anduril Cards.anduril

instance RunMessage Anduril where
  runMessage msg (Anduril a) = Anduril <$> runMessage msg a
