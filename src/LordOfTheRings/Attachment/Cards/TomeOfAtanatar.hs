module LordOfTheRings.Attachment.Cards.TomeOfAtanatar (tomeOfAtanatar) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Attachment.Record
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype TomeOfAtanatar = TomeOfAtanatar AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a leadership hero. Reduce the cost to play Tome of Atanatar by 1 for each
-- hero you control with a printed leadership resource icon.
-- Action: Discard Tome of Atanatar to play any leadership event card in your discard
-- pile as if it were in your hand. Then, place that event on the bottom of your deck.
tomeOfAtanatar :: AttachmentCard TomeOfAtanatar
tomeOfAtanatar = record TomeOfAtanatar Cards.tomeOfAtanatar Leadership

instance HasModifiersFor TomeOfAtanatar where
  getModifiersFor (TomeOfAtanatar a) = recordModifiers Leadership a
