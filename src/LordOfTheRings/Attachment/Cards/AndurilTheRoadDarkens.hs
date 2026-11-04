module LordOfTheRings.Attachment.Cards.AndurilTheRoadDarkens (andurilTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype AndurilTheRoadDarkens = AndurilTheRoadDarkens AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Lord of the Rings campaign, from The Ring Goes South.
andurilTheRoadDarkens :: AttachmentCard AndurilTheRoadDarkens
andurilTheRoadDarkens = attachment AndurilTheRoadDarkens Cards.andurilTheRoadDarkens

instance RunMessage AndurilTheRoadDarkens where
  runMessage msg (AndurilTheRoadDarkens a) = AndurilTheRoadDarkens <$> runMessage msg a
