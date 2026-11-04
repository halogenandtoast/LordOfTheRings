module LordOfTheRings.Attachment.Cards.EsquireOfRohan (esquireOfRohan) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype EsquireOfRohan = EsquireOfRohan AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Permanent. Setup: Attach to a hero. Its card def only, for The Battle of the Pelennor Fields to attach: the player
-- cards' own version gives its text.
esquireOfRohan :: AttachmentCard EsquireOfRohan
esquireOfRohan = attachment EsquireOfRohan Cards.esquireOfRohan

instance RunMessage EsquireOfRohan where
  runMessage msg (EsquireOfRohan a) = EsquireOfRohan <$> runMessage msg a
