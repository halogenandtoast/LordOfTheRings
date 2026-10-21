module LordOfTheRings.Attachment.Cards.BornAloft (bornAloft) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype BornAloft = BornAloft AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to an ally.
bornAloft :: AttachmentCard BornAloft
bornAloft = attachment BornAloft Cards.bornAloft & attachTo (AttachToAlly AnyAlly)

-- | Action: Discard Born Aloft from play to return attached ally to its owner's hand.
instance HasAbilities BornAloft where
  getAbilities (BornAloft a) = [mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

instance RunMessage BornAloft where
  runMessage msg s@(BornAloft a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      discard a
      withHost a returnToHand
      pure s
    _ -> BornAloft <$> runMessage msg a
