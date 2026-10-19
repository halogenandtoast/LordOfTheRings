module LordOfTheRings.Attachment.Cards.TheFavorOfTheLady (theFavorOfTheLady) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype TheFavorOfTheLady = TheFavorOfTheLady AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFavorOfTheLady :: AttachmentCard TheFavorOfTheLady
theFavorOfTheLady = attachment TheFavorOfTheLady Cards.theFavorOfTheLady

-- | Attached hero gains +1 willpower.
instance HasModifiersFor TheFavorOfTheLady where
  getModifiersFor (TheFavorOfTheLady a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1]
    _ -> pure ()
