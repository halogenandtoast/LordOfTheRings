module LordOfTheRings.Attachment.Cards.MithrilShirt (mithrilShirt) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype MithrilShirt = MithrilShirt AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (location), on its card def. Attach to a hero.
mithrilShirt :: AttachmentCard MithrilShirt
mithrilShirt = attachment MithrilShirt Cards.mithrilShirt

-- | Response: When attached hero is dealt any amount of damage, reduce that amount by 1.
instance HasAbilities MithrilShirt where
  getAbilities (MithrilShirt a) =
    [mkAbility a 1 $ response $ DealtDamageWindow When host | AttachedTo host <- [a.placement]]

instance RunMessage MithrilShirt where
  runMessage msg s@(MithrilShirt a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> reduceDamage host 1
      pure s
    _ -> MithrilShirt <$> runMessage msg a
