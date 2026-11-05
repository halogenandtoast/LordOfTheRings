module LordOfTheRings.Attachment.Cards.InnerStrength (innerStrength) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype InnerStrength = InnerStrength AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to the hero with The One Ring. Limit 1 per hero, on its card def.
innerStrength :: AttachmentCard InnerStrength
innerStrength = attachment InnerStrength Cards.innerStrength & attachTo (AttachToHero heroWithTheOneRing)

-- | Attached hero gets +1 defense.
instance HasModifiersFor InnerStrength where
  getModifiersFor (InnerStrength a) = withHost a \host -> modified a host [DefenseModifier 1]

-- | Response: Exhaust The One Ring and raise your threat by 1 to cancel a shadow effect just
-- triggered during an attack that attached hero is defending.
instance HasAbilities InnerStrength where
  getAbilities (InnerStrength a) =
    [ restricted a 1 (shadowEffectsCancelable <> HeroExists (HeroWithId hid <> DefendingHero))
        $ ResponseAbility (ShadowEffectWindow When) theOneRingCost
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage InnerStrength where
  runMessage msg s@(InnerStrength a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      cancelShadowEffects windows
      pure s
    _ -> InnerStrength <$> runMessage msg a
