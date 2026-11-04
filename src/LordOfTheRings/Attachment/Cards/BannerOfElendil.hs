module LordOfTheRings.Attachment.Cards.BannerOfElendil (bannerOfElendil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BannerOfElendil = BannerOfElendil AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to Aragorn. Its card def only, for The Battle of the Pelennor Fields to attach: the player
-- cards' own version gives its text.
bannerOfElendil :: AttachmentCard BannerOfElendil
bannerOfElendil = attachment BannerOfElendil Cards.bannerOfElendil

instance RunMessage BannerOfElendil where
  runMessage msg (BannerOfElendil a) = BannerOfElendil <$> runMessage msg a
