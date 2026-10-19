module LordOfTheRings.Attachment.Cards.DunedainMark (dunedainMark) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DunedainMark = DunedainMark AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainMark :: AttachmentCard DunedainMark
dunedainMark = attachment DunedainMark Cards.dunedainMark

-- | Attached hero gets +1 attack.
instance HasModifiersFor DunedainMark where
  getModifiersFor (DunedainMark a) = case a.placement of
    AttachedTo host -> modified a host [AttackModifier 1]
    _ -> pure ()

-- | Action: Pay 1 resource from attached hero's pool to attach Dúnedain Mark to another hero.
instance HasAbilities DunedainMark where
  getAbilities (DunedainMark a) =
    [ restricted a 1 (exists $ not_ $ HeroWithId hid) $ ActionAbility (ResourceCost host 1)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage DunedainMark where
  runMessage msg d@(DunedainMark attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      others <- case attrs.placement of
        AttachedTo (HeroTarget hid) -> select (not_ $ HeroWithId hid)
        _ -> pure []
      chooseOneM p do
        questionLabeled "dunedainMark.move"
        causedBy attrs
        targets others \h -> push $ AttachCard (toId attrs) (toTarget h)
      pure d
    _ -> DunedainMark <$> runMessage msg attrs
