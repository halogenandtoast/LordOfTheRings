module LordOfTheRings.Attachment.Cards.Cram (cram) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Cram = Cram AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
cram :: AttachmentCard Cram
cram = attachment Cram Cards.cram

-- | Action: Discard Cram to ready attached hero.
instance HasAbilities Cram where
  getAbilities (Cram a) = case a.placement of
    AttachedTo (HeroTarget hid) -> [restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero) $ ActionAbility Free]
    _ -> []

instance RunMessage Cram where
  runMessage msg s@(Cram a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      withHost a ready
      pure s
    _ -> Cram <$> runMessage msg a
