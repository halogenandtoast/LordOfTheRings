module LordOfTheRings.Attachment.Cards.HeirOfMardil (heirOfMardil) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noble))

newtype HeirOfMardil = HeirOfMardil AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

heirOfMardil :: AttachmentCard HeirOfMardil
heirOfMardil = attachment HeirOfMardil Cards.heirOfMardil & attachTo (AttachToHero $ HeroWithTrait Noble)

-- | Response: After attached hero gains any number of resources from a card effect,
-- exhaust Heir of Mardil to ready attached hero.
instance HasAbilities HeirOfMardil where
  getAbilities (HeirOfMardil a) =
    [ restricted a 1 (exists $ HeroWithId hid <> ExhaustedHero)
        $ ResponseAbility (GainedResourcesWindow After host) (ExhaustCost $ toTarget a)
    | AttachedTo host@(HeroTarget hid) <- [a.placement]
    ]

instance RunMessage HeirOfMardil where
  runMessage msg h@(HeirOfMardil attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs ready
      pure h
    _ -> HeirOfMardil <$> runMessage msg attrs
