module LordOfTheRings.Attachment.Cards.MithrilShirtTheRoadDarkens (mithrilShirtTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MithrilShirtTheRoadDarkens = MithrilShirtTheRoadDarkens AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Lord of the Rings campaign, from The Ring Goes South.
mithrilShirtTheRoadDarkens :: AttachmentCard MithrilShirtTheRoadDarkens
mithrilShirtTheRoadDarkens = attachment MithrilShirtTheRoadDarkens Cards.mithrilShirtTheRoadDarkens

instance RunMessage MithrilShirtTheRoadDarkens where
  runMessage msg (MithrilShirtTheRoadDarkens a) = MithrilShirtTheRoadDarkens <$> runMessage msg a
