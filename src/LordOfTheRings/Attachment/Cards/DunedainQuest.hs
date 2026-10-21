module LordOfTheRings.Attachment.Cards.DunedainQuest (dunedainQuest) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DunedainQuest = DunedainQuest AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainQuest :: AttachmentCard DunedainQuest
dunedainQuest = attachment DunedainQuest Cards.dunedainQuest

-- | Attached hero gets +1 willpower.
instance HasModifiersFor DunedainQuest where
  getModifiersFor (DunedainQuest a) = case a.placement of
    AttachedTo host -> modified a host [WillpowerModifier 1]
    _ -> pure ()

-- | Action: Pay 1 resource from attached hero's pool to attach Dúnedain Quest to another hero.
instance HasAbilities DunedainQuest where
  getAbilities (DunedainQuest a) =
    [ restricted a 1 (exists $ not_ $ HeroWithId hid) $ ActionAbility (ResourceCost host 1)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage DunedainQuest where
  runMessage msg d@(DunedainQuest attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      others <- case attrs.placement of
        AttachedTo (HeroTarget hid) -> select (not_ $ HeroWithId hid)
        _ -> pure []
      chooseOneM p do
        questionLabeled "moveSignal"
        causedBy attrs
        targets others $ moveAttachment attrs.id
      pure d
    _ -> DunedainQuest <$> runMessage msg attrs
