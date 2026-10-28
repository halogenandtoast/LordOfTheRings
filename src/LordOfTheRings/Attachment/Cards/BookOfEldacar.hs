module LordOfTheRings.Attachment.Cards.BookOfEldacar (bookOfEldacar) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Attachment.Record
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype BookOfEldacar = BookOfEldacar AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a tactics hero. Reduce the cost to play Book of Eldacar by 1 for each hero
-- you control with a printed tactics resource icon.
-- Action: Discard Book of Eldacar to play any tactics event card in your discard pile as
-- if it were in your hand. Then, place that event on the bottom of your deck.
bookOfEldacar :: AttachmentCard BookOfEldacar
bookOfEldacar = record BookOfEldacar Cards.bookOfEldacar Tactics

instance HasModifiersFor BookOfEldacar where
  getModifiersFor (BookOfEldacar a) = recordModifiers Tactics a
