module LordOfTheRings.Attachment.Cards.CitadelPlate (citadelPlate) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype CitadelPlate = CitadelPlate AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

citadelPlate :: AttachmentCard CitadelPlate
citadelPlate = attachment CitadelPlate Cards.citadelPlate

-- | Attached hero gets +4 hit points.
instance HasModifiersFor CitadelPlate where
  getModifiersFor (CitadelPlate a) = case a.placement of
    AttachedTo host -> modified a host [HitPointModifier 4]
    _ -> pure ()
