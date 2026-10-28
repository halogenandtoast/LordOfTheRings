module LordOfTheRings.Attachment.Cards.RingOfBarahir (ringOfBarahir) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))
import LordOfTheRings.Trait (Trait (Artifact))

newtype RingOfBarahir = RingOfBarahir AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
ringOfBarahir :: AttachmentCard RingOfBarahir
ringOfBarahir = attachment RingOfBarahir Cards.ringOfBarahir

-- | Attached hero gets +1 hit point for each Artifact attachment attached to it. If
-- attached hero is Aragorn, he also gains a lore resource icon.
instance HasModifiersFor RingOfBarahir where
  getModifiersFor (RingOfBarahir a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      artifacts <- length <$> attachedWithTrait Artifact host
      aragorn <- selectAny (HeroWithId hid <> HeroWithTitle "Aragorn")
      modified a host $ HitPointModifier artifacts : [GainResourceIcon Lore | aragorn]
    _ -> pure ()
