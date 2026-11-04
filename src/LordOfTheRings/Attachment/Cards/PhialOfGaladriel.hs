module LordOfTheRings.Attachment.Cards.PhialOfGaladriel (phialOfGaladriel) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype PhialOfGaladriel = PhialOfGaladriel AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Gift boon of The Lord of the Rings campaign, from Breaking of the Fellowship.
phialOfGaladriel :: AttachmentCard PhialOfGaladriel
phialOfGaladriel = attachment PhialOfGaladriel Cards.phialOfGaladriel

instance RunMessage PhialOfGaladriel where
  runMessage msg (PhialOfGaladriel a) = PhialOfGaladriel <$> runMessage msg a
