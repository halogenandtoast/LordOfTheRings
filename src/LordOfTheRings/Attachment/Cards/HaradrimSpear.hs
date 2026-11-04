module LordOfTheRings.Attachment.Cards.HaradrimSpear (haradrimSpear) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Trait (Trait (Harad))

newtype HaradrimSpear = HaradrimSpear AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted, on its card def. Attach to a Harad character (the trait the card means by
-- "Haradrim").
haradrimSpear :: AttachmentCard HaradrimSpear
haradrimSpear =
  attachment HaradrimSpear Cards.haradrimSpear
    & attachTo (AttachToCharacterMatching (HeroWithTrait Harad) (AllyWithTrait Harad))

-- | Attached character gets +1 attack.
instance HasModifiersFor HaradrimSpear where
  getModifiersFor (HaradrimSpear a) = withHost a \host -> modified a host [AttackModifier 1]

-- | Response: After attached character is declared as an attacker, it gets +3 attack for
-- this attack. After this attack resolves, discard Haradrim Spear.
instance HasAbilities HaradrimSpear where
  getAbilities (HaradrimSpear a) =
    [mkAbility a 1 $ response $ AttackerDeclaredWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage HaradrimSpear where
  runMessage msg s@(HaradrimSpear a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> attackModifier a host (AttackModifier 3)
      phaseModifier a a DiscardAfterAttack
      pure s
    StopAttacking host | AttachedTo host == a.placement -> do
      leaves <- elem DiscardAfterAttack <$> getModifiers a
      when leaves $ discard a
      HaradrimSpear <$> runMessage msg a
    _ -> HaradrimSpear <$> runMessage msg a
