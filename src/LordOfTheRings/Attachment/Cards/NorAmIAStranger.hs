module LordOfTheRings.Attachment.Cards.NorAmIAStranger (norAmIAStranger) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype NorAmIAStranger = NorAmIAStranger AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
norAmIAStranger :: AttachmentCard NorAmIAStranger
norAmIAStranger = attachment NorAmIAStranger Cards.norAmIAStranger & attachTo AttachToCharacter

-- | Attached character gains the Rohan trait.
instance HasModifiersFor NorAmIAStranger where
  getModifiersFor (NorAmIAStranger a) = withHost a \host -> modified a host [GainTrait Rohan]
