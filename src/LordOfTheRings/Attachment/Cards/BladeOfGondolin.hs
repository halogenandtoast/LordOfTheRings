module LordOfTheRings.Attachment.Cards.BladeOfGondolin (bladeOfGondolin) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BladeOfGondolin = BladeOfGondolin AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

bladeOfGondolin :: AttachmentCard BladeOfGondolin
bladeOfGondolin = attachment BladeOfGondolin Cards.bladeOfGondolin
