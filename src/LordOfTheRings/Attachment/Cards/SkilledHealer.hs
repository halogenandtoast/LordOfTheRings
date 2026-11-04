module LordOfTheRings.Attachment.Cards.SkilledHealer (skilledHealer) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Healer))

newtype SkilledHealer = SkilledHealer AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Black Riders. Permanent, on its card def. Setup: Attach to a hero: as the
-- Campaign Pool records it on its hero.
skilledHealer :: AttachmentCard SkilledHealer
skilledHealer = attachment SkilledHealer Cards.skilledHealer & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +2 hit points and gains the Healer trait.
instance HasModifiersFor SkilledHealer where
  getModifiersFor (SkilledHealer a) = withHost a \host -> modified a host [HitPointModifier 2, GainTrait Healer]
