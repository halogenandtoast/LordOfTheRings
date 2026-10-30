module LordOfTheRings.Attachment.Cards.ElfFriend (elfFriend) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype ElfFriend = ElfFriend AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
elfFriend :: AttachmentCard ElfFriend
elfFriend = attachment ElfFriend Cards.elfFriend & attachTo AttachToCharacter

-- | Attached character gains the Noldor and Silvan traits.
instance HasModifiersFor ElfFriend where
  getModifiersFor (ElfFriend a) = withHost a \host -> modified a host [GainTrait Noldor, GainTrait Silvan]
