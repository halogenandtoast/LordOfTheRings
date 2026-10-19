module LordOfTheRings.Attachment.Cards.HornOfGondor (hornOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype HornOfGondor = HornOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hornOfGondor :: AttachmentCard HornOfGondor
hornOfGondor = attachment HornOfGondor Cards.hornOfGondor

-- | Response: After a character is destroyed, add 1 resource to attached hero's pool.
instance HasAbilities HornOfGondor where
  getAbilities (HornOfGondor a) = [mkAbility a 1 $ response $ CharacterDestroyedWindow After | AttachedTo _ <- [a.placement]]

instance RunMessage HornOfGondor where
  runMessage msg s@(HornOfGondor a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> HornOfGondor <$> runMessage msg a
