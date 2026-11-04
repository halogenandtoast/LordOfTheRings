module LordOfTheRings.Attachment.Cards.ThreeGoldenHairs (threeGoldenHairs) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ThreeGoldenHairs = ThreeGoldenHairs AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Lord of the Rings campaign, from Breaking of the Fellowship.
threeGoldenHairs :: AttachmentCard ThreeGoldenHairs
threeGoldenHairs = attachment ThreeGoldenHairs Cards.threeGoldenHairs

instance RunMessage ThreeGoldenHairs where
  runMessage msg (ThreeGoldenHairs a) = ThreeGoldenHairs <$> runMessage msg a
