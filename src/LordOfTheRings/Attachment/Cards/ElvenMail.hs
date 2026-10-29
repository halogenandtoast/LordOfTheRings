module LordOfTheRings.Attachment.Cards.ElvenMail (elvenMail) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype ElvenMail = ElvenMail AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted. Attach to a Noldor or Silvan character.
elvenMail :: AttachmentCard ElvenMail
elvenMail =
  attachment ElvenMail Cards.elvenMail
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan])
          (AllyMatchAny [AllyWithTrait Noldor, AllyWithTrait Silvan])
      )

-- | Attached character gets +2 hit points and gains sentinel.
instance HasModifiersFor ElvenMail where
  getModifiersFor (ElvenMail a) = withHost a \host -> modified a host [HitPointModifier 2, GainKeyword Sentinel]
