module LordOfTheRings.Attachment.Cards.DunedainWarning (dunedainWarning) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DunedainWarning = DunedainWarning AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainWarning :: AttachmentCard DunedainWarning
dunedainWarning = attachment DunedainWarning Cards.dunedainWarning

-- | Attached hero gets +1 defense.
instance HasModifiersFor DunedainWarning where
  getModifiersFor (DunedainWarning a) = case a.placement of
    AttachedTo host -> modified a host [DefenseModifier 1]
    _ -> pure ()

-- | Action: Pay 1 resource from attached hero's pool to attach Dúnedain Warning to another hero.
instance HasAbilities DunedainWarning where
  getAbilities (DunedainWarning a) =
    [ restricted a 1 (exists $ not_ $ HeroWithId hid) $ ActionAbility (ResourceCost host 1)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage DunedainWarning where
  runMessage msg d@(DunedainWarning attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      others <- case attrs.placement of
        AttachedTo (HeroTarget hid) -> select (not_ $ HeroWithId hid)
        _ -> pure []
      chooseOneM p do
        questionLabeled "moveSignal"
        causedBy attrs
        targets others $ moveAttachment attrs.id
      pure d
    _ -> DunedainWarning <$> runMessage msg attrs
