module LordOfTheRings.Attachment.Cards.StingTheRoadDarkens (stingTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype StingTheRoadDarkens = StingTheRoadDarkens AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Lord of the Rings campaign, from The Ring Goes South.
stingTheRoadDarkens :: AttachmentCard StingTheRoadDarkens
stingTheRoadDarkens = attachment StingTheRoadDarkens Cards.stingTheRoadDarkens

instance RunMessage StingTheRoadDarkens where
  runMessage msg (StingTheRoadDarkens a) = StingTheRoadDarkens <$> runMessage msg a
