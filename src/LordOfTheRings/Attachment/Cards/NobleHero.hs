module LordOfTheRings.Attachment.Cards.NobleHero (nobleHero) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noble))

newtype NobleHero = NobleHero AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Black Riders. Permanent, on its card def. Setup: Attach to a hero: as the
-- Campaign Pool records it on its hero.
nobleHero :: AttachmentCard NobleHero
nobleHero = attachment NobleHero Cards.nobleHero & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +1 [willpower] and gains the Noble trait.
instance HasModifiersFor NobleHero where
  getModifiersFor (NobleHero a) = withHost a \host -> modified a host [WillpowerModifier 1, GainTrait Noble]
