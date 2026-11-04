module LordOfTheRings.Attachment.Cards.GlamdringTheRoadDarkens (glamdringTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype GlamdringTheRoadDarkens = GlamdringTheRoadDarkens AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Lord of the Rings campaign, from The Ring Goes South.
glamdringTheRoadDarkens :: AttachmentCard GlamdringTheRoadDarkens
glamdringTheRoadDarkens = attachment GlamdringTheRoadDarkens Cards.glamdringTheRoadDarkens

instance RunMessage GlamdringTheRoadDarkens where
  runMessage msg (GlamdringTheRoadDarkens a) = GlamdringTheRoadDarkens <$> runMessage msg a
