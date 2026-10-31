module LordOfTheRings.Attachment.Cards.Stalwart (stalwart) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType), cdCardType, toCardDef)
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

-- ponytail: played at no cost, as attaching from the discard pile does
instance RunMessage Stalwart where
  runMessage msg s@(Stalwart a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discarded <- (.discard) <$> getPlayer pid
      let attachments = [pc | PlayerCard pc <- discarded, cdCardType (toCardDef pc) == AttachmentType]
      withHost a \host -> chooseOrRunOneM pid do
        questionLabeled "stalwart.playAttachment"
        causedBy a
        targets attachments \pc -> attachFromDiscard pid pc host
      pure s
    _ -> Stalwart <$> runMessage msg a
