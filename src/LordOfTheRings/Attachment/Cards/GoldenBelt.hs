module LordOfTheRings.Attachment.Cards.GoldenBelt (goldenBelt) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype GoldenBelt = GoldenBelt AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Limit 1 per hero.
goldenBelt :: AttachmentCard GoldenBelt
goldenBelt = attachment GoldenBelt Cards.goldenBelt

-- | Attached hero can have 1 additional restricted attachment.
instance HasModifiersFor GoldenBelt where
  getModifiersFor (GoldenBelt a) = case a.placement of
    AttachedTo host -> modified a host [AdditionalRestrictedSlots 1]
    _ -> pure ()

-- | Its hero, once it is gone, keeps no more than 2 restricted attachments.
instance RunMessage GoldenBelt where
  runMessage msg (GoldenBelt a) = case msg of
    _ | AttachedTo host <- a.placement, leaves host -> do
      push $ CheckRestricted host
      GoldenBelt <$> runMessage msg a
    _ -> GoldenBelt <$> runMessage msg a
   where
    leaves host = case msg of
      Discard _ t -> isTarget a t
      ReturnToHand t -> isTarget a t
      ShuffleIntoOwnersDeck t -> isTarget a t
      AttachCard aid new -> aid == a.id && new /= host
      _ -> False
