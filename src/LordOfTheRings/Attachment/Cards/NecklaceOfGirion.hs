module LordOfTheRings.Attachment.Cards.NecklaceOfGirion (necklaceOfGirion) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype NecklaceOfGirion = NecklaceOfGirion AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy or location), as its card def's keyword. Attach to a hero.
necklaceOfGirion :: AttachmentCard NecklaceOfGirion
necklaceOfGirion = attachment NecklaceOfGirion Cards.necklaceOfGirion

-- | Attached hero gets +2 willpower and collects an additional resource during the
-- resource phase.
instance HasModifiersFor NecklaceOfGirion where
  getModifiersFor (NecklaceOfGirion a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 2, CollectsAdditionalResources 1]
    _ -> pure ()
