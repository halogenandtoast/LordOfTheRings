module LordOfTheRings.Attachment.Cards.CelebriansStone (celebriansStone) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype CelebriansStone = CelebriansStone AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

celebriansStone :: AttachmentCard CelebriansStone
celebriansStone = attachment CelebriansStone Cards.celebriansStone

instance HasModifiersFor CelebriansStone where
  getModifiersFor (CelebriansStone a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 2]
    _ -> pure ()
