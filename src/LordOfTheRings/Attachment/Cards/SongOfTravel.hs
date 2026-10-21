module LordOfTheRings.Attachment.Cards.SongOfTravel (songOfTravel) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype SongOfTravel = SongOfTravel AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

songOfTravel :: AttachmentCard SongOfTravel
songOfTravel = attachment SongOfTravel Cards.songOfTravel

-- | Attached hero gains a Spirit resource icon.
instance HasModifiersFor SongOfTravel where
  getModifiersFor (SongOfTravel a) = withHost a \host -> modified a host [GainResourceIcon Spirit]
