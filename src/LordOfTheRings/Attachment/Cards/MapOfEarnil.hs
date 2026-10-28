module LordOfTheRings.Attachment.Cards.MapOfEarnil (mapOfEarnil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Attachment.Record
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype MapOfEarnil = MapOfEarnil AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a spirit hero. Reduce the cost to play Map of Earnil by 1 for each hero you
-- control with a printed spirit resource icon.
-- Action: Discard Map of Earnil to play any spirit event card in your discard pile as if
-- it were in your hand. Then, place that event on the bottom of your deck.
mapOfEarnil :: AttachmentCard MapOfEarnil
mapOfEarnil = record MapOfEarnil Cards.mapOfEarnil Spirit

instance HasModifiersFor MapOfEarnil where
  getModifiersFor (MapOfEarnil a) = recordModifiers Spirit a
