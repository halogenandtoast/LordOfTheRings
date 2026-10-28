module LordOfTheRings.Attachment.Cards.ScrollOfIsildur (scrollOfIsildur) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Attachment.Record
import LordOfTheRings.Sphere (Sphere (Lore))

newtype ScrollOfIsildur = ScrollOfIsildur AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a lore hero. Reduce the cost to play Scroll of Isildur by 1 for each hero
-- you control with a printed lore resource icon.
-- Action: Discard Scroll of Isildur to play any lore event card in your discard pile as
-- if it were in your hand. Then, place that event on the bottom of your deck.
scrollOfIsildur :: AttachmentCard ScrollOfIsildur
scrollOfIsildur = record ScrollOfIsildur Cards.scrollOfIsildur Lore

instance HasModifiersFor ScrollOfIsildur where
  getModifiersFor (ScrollOfIsildur a) = recordModifiers Lore a
