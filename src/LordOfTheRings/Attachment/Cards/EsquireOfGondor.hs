module LordOfTheRings.Attachment.Cards.EsquireOfGondor (esquireOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype EsquireOfGondor = EsquireOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Permanent. Setup: Attach to a hero. Its card def only, for The Battle of the Pelennor Fields to attach: the player
-- cards' own version gives its text.
esquireOfGondor :: AttachmentCard EsquireOfGondor
esquireOfGondor = attachment EsquireOfGondor Cards.esquireOfGondor

instance RunMessage EsquireOfGondor where
  runMessage msg (EsquireOfGondor a) = EsquireOfGondor <$> runMessage msg a
