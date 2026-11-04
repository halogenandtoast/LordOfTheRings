module LordOfTheRings.Attachment.Cards.FiresideSong (firesideSong) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit, Song))

newtype FiresideSong = FiresideSong AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit hero. Limit 1 per hero, on its card def.
firesideSong :: AttachmentCard FiresideSong
firesideSong = attachment FiresideSong Cards.firesideSong & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | Attached hero gets +1 willpower for each Song attached to it.
instance HasModifiersFor FiresideSong where
  getModifiersFor (FiresideSong a) = withHost a \host -> do
    songs <- length <$> attachedWithTrait Song host
    modified a host [WillpowerModifier songs]
