module LordOfTheRings.Attachment.Cards.RoundShield (roundShield) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype RoundShield = RoundShield AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a character. Restricted, on its card def.
roundShield :: AttachmentCard RoundShield
roundShield = attachment RoundShield Cards.roundShield & attachTo AttachToCharacter

-- | Response: After a shadow effect is triggered during combat, exhaust Round Shield to
-- give attached character +2 defense for this attack.
instance HasAbilities RoundShield where
  getAbilities (RoundShield a) =
    [mkAbility a 1 $ ResponseAbility (AnyShadowEffectResolvedWindow After) (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage RoundShield where
  runMessage msg s@(RoundShield a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> attackModifier a host (DefenseModifier 2)
      pure s
    _ -> RoundShield <$> runMessage msg a
