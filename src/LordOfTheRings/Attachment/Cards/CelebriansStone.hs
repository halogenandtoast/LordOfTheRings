module LordOfTheRings.Attachment.Cards.CelebriansStone (celebriansStone) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype CelebriansStone = CelebriansStone AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

celebriansStone :: AttachmentCard CelebriansStone
celebriansStone = attachment CelebriansStone Cards.celebriansStone

-- | Attached hero gains +2 willpower. If attached hero is Aragorn, he also gains a
-- Spirit resource icon.
instance HasModifiersFor CelebriansStone where
  getModifiersFor (CelebriansStone a) = case a.placement of
    AttachedTo host@(HeroTarget hid) -> do
      aragorn <- (\(Name title _) -> title == "Aragorn") . (.name) <$> getHero hid
      modified a host $ WillpowerModifier 2 : [GainResourceIcon Spirit | aragorn]
    AttachedTo host -> modified a host [WillpowerModifier 2]
    _ -> pure ()
