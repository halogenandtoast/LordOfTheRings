module LordOfTheRings.Attachment.Cards.ABurningBrand (aBurningBrand) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype ABurningBrand = ABurningBrand AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Lore character. Restricted.
aBurningBrand :: AttachmentCard ABurningBrand
aBurningBrand =
  attachment ABurningBrand Cards.aBurningBrand & attachTo (AttachToCharacterMatching (HeroWithSphere Lore) (AllyWithSphere Lore))

-- | Response: Exhaust A Burning Brand to cancel a shadow effect just triggered during an
-- attack that attached character is defending.
instance HasAbilities ABurningBrand where
  getAbilities (ABurningBrand a) =
    [ restricted a 1 EncounterEffectsCancelable $ ResponseAbility (ShadowEffectAgainstWindow When host) (exhaustCost a)
    | AttachedTo host <- [a.placement]
    ]

instance RunMessage ABurningBrand where
  runMessage msg s@(ABurningBrand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      cancelShadowEffects windows
      pure s
    _ -> ABurningBrand <$> runMessage msg a
