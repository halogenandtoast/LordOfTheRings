module LordOfTheRings.Attachment.Cards.InServiceOfTheSteward (inServiceOfTheSteward) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype InServiceOfTheSteward = InServiceOfTheSteward AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character.
inServiceOfTheSteward :: AttachmentCard InServiceOfTheSteward
inServiceOfTheSteward = attachment InServiceOfTheSteward Cards.inServiceOfTheSteward & attachTo AttachToCharacter

-- | Attached character gains the Gondor trait.
instance HasModifiersFor InServiceOfTheSteward where
  getModifiersFor (InServiceOfTheSteward a) = withHost a \host -> modified a host [GainTrait Gondor]
