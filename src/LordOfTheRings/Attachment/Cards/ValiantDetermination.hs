module LordOfTheRings.Attachment.Cards.ValiantDetermination (valiantDetermination) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ValiantDetermination = ValiantDetermination AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to an ally.
valiantDetermination :: AttachmentCard ValiantDetermination
valiantDetermination = attachment ValiantDetermination Cards.valiantDetermination & attachTo (AttachToAlly AnyAlly)

-- | Attached ally does not exhaust to quest.
instance HasModifiersFor ValiantDetermination where
  getModifiersFor (ValiantDetermination a) = withHost a \host -> modified a host [DoesNotExhaustToCommit]
