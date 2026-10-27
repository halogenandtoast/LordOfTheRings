module LordOfTheRings.Attachment.Cards.RingMail (ringMail) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf, Hobbit))

newtype RingMail = RingMail AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf or Hobbit character. Restricted, as its card def says.
ringMail :: AttachmentCard RingMail
ringMail =
  attachment RingMail Cards.ringMail
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dwarf, HeroWithTrait Hobbit])
          (AllyMatchAny [AllyWithTrait Dwarf, AllyWithTrait Hobbit])
      )

-- | Attached character gets +1 hit point and +1 defense.
instance HasModifiersFor RingMail where
  getModifiersFor (RingMail a) = withHost a \host -> modified a host [HitPointModifier 1, DefenseModifier 1]
