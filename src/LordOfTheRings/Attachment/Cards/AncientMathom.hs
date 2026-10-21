module LordOfTheRings.Attachment.Cards.AncientMathom (ancientMathom) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype AncientMathom = AncientMathom AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ancientMathom :: AttachmentCard AncientMathom
ancientMathom = attachment AncientMathom Cards.ancientMathom & attachTo (AttachToLocation AnyLocation)

-- | Response: After attached location is explored, the first player draws 3 cards.
instance HasAbilities AncientMathom where
  getAbilities (AncientMathom a) =
    [mkAbility a 1 $ response $ ExploredWindow When lid | AttachedTo (LocationTarget lid) <- [a.placement]]

instance RunMessage AncientMathom where
  runMessage msg s@(AncientMathom a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      drawCards lead 3
      pure s
    _ -> AncientMathom <$> runMessage msg a
