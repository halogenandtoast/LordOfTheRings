module LordOfTheRings.Attachment.Cards.StoneOfElostirion (stoneOfElostirion) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype StoneOfElostirion = StoneOfElostirion AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (location) and Restricted, on its card def. Attach to a hero.
stoneOfElostirion :: AttachmentCard StoneOfElostirion
stoneOfElostirion = attachment StoneOfElostirion Cards.stoneOfElostirion

-- | Attached hero gets +2 willpower. You draw 1 additional card during the resource phase.
instance HasModifiersFor StoneOfElostirion where
  getModifiersFor (StoneOfElostirion a) = case a.placement of
    AttachedTo host -> do
      modified a host [WillpowerModifier 2]
      modified a (PlayerTarget a.controller) [AdditionalResourcePhaseDraws 1]
    _ -> pure ()
