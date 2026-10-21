module LordOfTheRings.Attachment.Cards.SongOfKings (songOfKings) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))

newtype SongOfKings = SongOfKings AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

songOfKings :: AttachmentCard SongOfKings
songOfKings = attachment SongOfKings Cards.songOfKings

-- | Attached hero gains a Leadership resource icon.
instance HasModifiersFor SongOfKings where
  getModifiersFor (SongOfKings a) = withHost a \host -> modified a host [GainResourceIcon Leadership]
