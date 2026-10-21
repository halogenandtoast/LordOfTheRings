module LordOfTheRings.Attachment.Cards.SongOfWisdom (songOfWisdom) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype SongOfWisdom = SongOfWisdom AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

songOfWisdom :: AttachmentCard SongOfWisdom
songOfWisdom = attachment SongOfWisdom Cards.songOfWisdom

-- | Attached hero gains a Lore resource icon.
instance HasModifiersFor SongOfWisdom where
  getModifiersFor (SongOfWisdom a) = withHost a \host -> modified a host [GainResourceIcon Lore]
