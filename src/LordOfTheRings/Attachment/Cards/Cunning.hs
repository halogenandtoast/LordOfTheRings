module LordOfTheRings.Attachment.Cards.Cunning (cunning) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Cunning = Cunning AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Mysterious Omens. Permanent.
cunning :: AttachmentCard Cunning
cunning = attachment Cunning Cards.cunning

-- | Action: Attached hero gets +2 willpower, +2 attack, and +2 defense until the end of the
-- phase. (Limit once per game.)
instance HasAbilities Cunning where
  getAbilities (Cunning a) = [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ ActionAbility Free | AttachedTo _ <- [a.placement]]

-- | Setup: Attach to a hero. Add 2 resources to that hero's resource pool. Attached as the
-- scenario sets up (step 4), it adds them as the players' setup goes on.
instance RunMessage Cunning where
  runMessage msg s@(Cunning a) = case msg of
    DoStep 5 Setup -> do
      withHost a \host -> placeResources a host 2
      pure s
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> phaseModifiers a host [WillpowerModifier 2, AttackModifier 2, DefenseModifier 2]
      pure s
    _ -> Cunning <$> runMessage msg a
