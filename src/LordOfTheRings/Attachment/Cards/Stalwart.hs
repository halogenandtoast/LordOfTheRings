module LordOfTheRings.Attachment.Cards.Stalwart (stalwart) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype Stalwart = Stalwart AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of the Angmar Awakened campaign, from Mysterious Omens. Permanent.
stalwart :: AttachmentCard Stalwart
stalwart = attachment Stalwart Cards.stalwart

-- | Attached hero gets +1 defense.
instance HasModifiersFor Stalwart where
  getModifiersFor (Stalwart a) = case a.placement of
    AttachedTo host -> modified a host [DefenseModifier 1]
    _ -> pure ()

-- | Planning Action: Play an attachment from your discard pile on attached hero. (Limit
-- once per game.)
instance HasAbilities Stalwart where
  getAbilities (Stalwart a) =
    [limited (GroupLimit PerGame 1) $ restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility Free | AttachedTo _ <- [a.placement]]

-- | Played, the attachment is paid for, the hero's discounts on it read.
instance RunMessage Stalwart where
  runMessage msg s@(Stalwart a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a $ playAttachmentFromDiscardOn pid a
      pure s
    _ -> Stalwart <$> runMessage msg a
