module LordOfTheRings.Attachment.Cards.LeafWrappedLembas (leafWrappedLembas) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype LeafWrappedLembas = LeafWrappedLembas AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Lord of the Rings campaign, from Breaking of the Fellowship.
leafWrappedLembas :: AttachmentCard LeafWrappedLembas
leafWrappedLembas = attachment LeafWrappedLembas Cards.leafWrappedLembas

instance RunMessage LeafWrappedLembas where
  runMessage msg (LeafWrappedLembas a) = LeafWrappedLembas <$> runMessage msg a
