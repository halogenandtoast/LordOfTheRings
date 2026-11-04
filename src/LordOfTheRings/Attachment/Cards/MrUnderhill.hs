module LordOfTheRings.Attachment.Cards.MrUnderhill (mrUnderhill) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MrUnderhill = MrUnderhill AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon. Permanent. Setup: Attach to the Ring-bearer, which A Shadow of the Past's
-- campaign card does. Its Action is The Black Riders' player cards' to give.
mrUnderhill :: AttachmentCard MrUnderhill
mrUnderhill = attachment MrUnderhill Cards.mrUnderhill

instance RunMessage MrUnderhill where
  runMessage msg (MrUnderhill a) = MrUnderhill <$> runMessage msg a
