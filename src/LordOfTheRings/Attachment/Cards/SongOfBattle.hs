module LordOfTheRings.Attachment.Cards.SongOfBattle (songOfBattle) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Tactics))

newtype SongOfBattle = SongOfBattle AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

songOfBattle :: AttachmentCard SongOfBattle
songOfBattle = attachment SongOfBattle Cards.songOfBattle

-- | Attached hero gains a Tactics resource icon.
instance HasModifiersFor SongOfBattle where
  getModifiersFor (SongOfBattle a) = withHost a \host -> modified a host [GainResourceIcon Tactics]
