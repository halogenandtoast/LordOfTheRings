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
-- attack that attached character is defending. (Errata; as printed, "While attached
-- character is defending, cancel any shadow effects on cards dealt to the attacking
-- enemy.")
instance HasAbilities ABurningBrand where
  getAbilities (ABurningBrand a) = case a.placement of
    AttachedTo host ->
      let shadow = restricted a 1 shadowEffectsCancelable
       in errataOr
            a.owner
            (shadow $ ResponseAbility (ShadowEffectAgainstWindow When host) (exhaustCost a))
            (shadow $ silent (ShadowEffectAgainstWindow When host))
    _ -> []

instance RunMessage ABurningBrand where
  runMessage msg s@(ABurningBrand a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      cancelShadowEffects windows
      pure s
    _ -> ABurningBrand <$> runMessage msg a
