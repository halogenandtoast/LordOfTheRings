module LordOfTheRings.Attachment.Cards.TheFavorOfTheLady (theFavorOfTheLady) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype TheFavorOfTheLady = TheFavorOfTheLady AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFavorOfTheLady :: AttachmentCard TheFavorOfTheLady
theFavorOfTheLady = attachment TheFavorOfTheLady Cards.theFavorOfTheLady
