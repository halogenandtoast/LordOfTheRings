module LordOfTheRings.Attachment.Cards.Hardy (hardy) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Hardy = Hardy AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Mysterious Omens. Permanent.
hardy :: AttachmentCard Hardy
hardy = attachment Hardy Cards.hardy

-- | Attached hero gets +2 hit points.
instance HasModifiersFor Hardy where
  getModifiersFor (Hardy a) = case a.placement of
    AttachedTo host -> modified a host [HitPointModifier 2]
    _ -> pure ()

-- | Action: Ready attached hero. (Limit once per game.)
instance HasAbilities Hardy where
  getAbilities (Hardy a) = [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage Hardy where
  runMessage msg s@(Hardy a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> Hardy <$> runMessage msg a
