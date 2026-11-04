module LordOfTheRings.Attachment.Cards.LegacyBlade (legacyBlade) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype LegacyBlade = LegacyBlade AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero. Restricted, on its card def.
legacyBlade :: AttachmentCard LegacyBlade
legacyBlade = attachment LegacyBlade Cards.legacyBlade

-- | Attached hero gets +1 attack for each side quest in the victory display. (Limit +3
-- attack.)
instance HasModifiersFor LegacyBlade where
  getModifiersFor (LegacyBlade a) = withHost a \host -> do
    x <- min 3 <$> getSideQuestsInVictoryDisplay
    when (x > 0) $ modified a host [AttackModifier x]
