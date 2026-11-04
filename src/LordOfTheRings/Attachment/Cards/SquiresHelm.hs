module LordOfTheRings.Attachment.Cards.SquiresHelm (squiresHelm) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype SquiresHelm = SquiresHelm AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to an ally. Restricted. Limit 1 per ally.
squiresHelm :: AttachmentCard SquiresHelm
squiresHelm = attachment SquiresHelm Cards.squiresHelm & attachTo (AttachToAlly AnyAlly)

-- | Attached ally gets +2 hit points.
instance HasModifiersFor SquiresHelm where
  getModifiersFor (SquiresHelm a) = case a.placement of
    AttachedTo host -> modified a host [HitPointModifier 2]
    _ -> pure ()
