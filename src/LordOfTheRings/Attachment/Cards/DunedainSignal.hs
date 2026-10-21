module LordOfTheRings.Attachment.Cards.DunedainSignal (dunedainSignal) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))

newtype DunedainSignal = DunedainSignal AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainSignal :: AttachmentCard DunedainSignal
dunedainSignal = attachment DunedainSignal Cards.dunedainSignal

-- | Attached hero gets sentinel.
instance HasModifiersFor DunedainSignal where
  getModifiersFor (DunedainSignal a) = case a.placement of
    AttachedTo host -> modified a host [GainKeyword Sentinel]
    _ -> pure ()

-- | Action: Pay 1 resource from attached hero's pool to attach Dúnedain Signal to another hero.
instance HasAbilities DunedainSignal where
  getAbilities (DunedainSignal a) =
    [ restricted a 1 (exists $ not_ $ HeroWithId hid) $ ActionAbility (ResourceCost host 1)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage DunedainSignal where
  runMessage msg d@(DunedainSignal attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      others <- case attrs.placement of
        AttachedTo (HeroTarget hid) -> select (not_ $ HeroWithId hid)
        _ -> pure []
      chooseOneM p do
        questionLabeled "moveSignal"
        causedBy attrs
        targets others $ moveAttachment attrs.id
      pure d
    _ -> DunedainSignal <$> runMessage msg attrs
