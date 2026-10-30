module LordOfTheRings.Attachment.Cards.DunedainRemedy (dunedainRemedy) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DunedainRemedy = DunedainRemedy AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
dunedainRemedy :: AttachmentCard DunedainRemedy
dunedainRemedy = attachment DunedainRemedy Cards.dunedainRemedy

-- | Response: After Dúnedain Remedy is attached to a hero, heal 1 damage on that hero.
-- Action: Pay 1 resource from attached hero's resource pool to attach Dúnedain Remedy to
-- another hero.
instance HasAbilities DunedainRemedy where
  getAbilities (DunedainRemedy a) =
    mkAbility a 1 (response $ AttachedToHeroWindow After (toTarget a))
      : [ restricted a 2 (exists $ not_ $ HeroWithId hid) $ ActionAbility (ResourceCost host 1)
        | AttachedTo host@(HeroTarget hid) <- [a.placement]
        ]

instance RunMessage DunedainRemedy where
  runMessage msg d@(DunedainRemedy attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \host -> healDamage attrs host 1
      pure d
    UseCardAbility p (isSource attrs -> True) 2 _ -> do
      others <- case attrs.placement of
        AttachedTo (HeroTarget hid) -> select (not_ $ HeroWithId hid)
        _ -> pure []
      chooseOneM p do
        questionLabeled "moveSignal"
        causedBy attrs
        targets others $ moveAttachment attrs.id
      pure d
    _ -> DunedainRemedy <$> runMessage msg attrs
