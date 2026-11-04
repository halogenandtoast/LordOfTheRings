module LordOfTheRings.Attachment.Cards.LorienRope (lorienRope) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype LorienRope = LorienRope AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Lord of the Rings campaign, from Breaking of the Fellowship.
lorienRope :: AttachmentCard LorienRope
lorienRope = attachment LorienRope Cards.lorienRope

instance RunMessage LorienRope where
  runMessage msg (LorienRope a) = LorienRope <$> runMessage msg a
