module LordOfTheRings.Attachment.Cards.SelfPreservation (selfPreservation) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype SelfPreservation = SelfPreservation AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

selfPreservation :: AttachmentCard SelfPreservation
selfPreservation = attachment SelfPreservation Cards.selfPreservation & attachTo (AttachToCharacter)
