module LordOfTheRings.Attachment.Cards.DunedainCache (dunedainCache) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype DunedainCache = DunedainCache AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

dunedainCache :: AttachmentCard DunedainCache
dunedainCache = attachment DunedainCache Cards.dunedainCache

-- | Attached hero gets ranged.
instance HasModifiersFor DunedainCache where
  getModifiersFor (DunedainCache a) = case a.placement of
    AttachedTo host -> modified a host [GainKeyword Ranged]
    _ -> pure ()

-- | Action: Pay 1 resource from attached hero's pool to attach Dúnedain Cache to another hero.
instance HasAbilities DunedainCache where
  getAbilities (DunedainCache a) =
    [ restricted a 1 (exists $ not_ $ HeroWithId hid) $ ActionAbility (ResourceCost host 1)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage DunedainCache where
  runMessage msg d@(DunedainCache attrs) = case msg of
    UseCardAbility p (isSource attrs -> True) 1 _ -> do
      others <- case attrs.placement of
        AttachedTo (HeroTarget hid) -> select (not_ $ HeroWithId hid)
        _ -> pure []
      chooseOneM p do
        questionLabeled "moveSignal"
        causedBy attrs
        targets others $ moveAttachment attrs.id
      pure d
    _ -> DunedainCache <$> runMessage msg attrs
