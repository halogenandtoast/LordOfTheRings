module LordOfTheRings.Attachment.Cards.VigilantGuard (vigilantGuard) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype VigilantGuard = VigilantGuard AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Warrior character. Limit 1 per character, on its card def.
vigilantGuard :: AttachmentCard VigilantGuard
vigilantGuard =
  attachment VigilantGuard Cards.vigilantGuard
    & attachTo (AttachToCharacterMatching (HeroWithTrait Warrior) (AllyWithTrait Warrior))

-- | Attached character gets +2 hit points.
instance HasModifiersFor VigilantGuard where
  getModifiersFor (VigilantGuard a) = withHost a \host -> modified a host [HitPointModifier 2]

-- | Response: When another character would be assigned any amount of damage, place 1 of
-- that damage on attached character instead.
instance HasAbilities VigilantGuard where
  getAbilities (VigilantGuard a) =
    [mkAbility a 1 $ response $ AnotherCharacterDealtDamageWindow When host | AttachedTo host <- [a.placement]]

instance RunMessage VigilantGuard where
  runMessage msg s@(VigilantGuard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ (take 1 [t | Window _ (DealtDamage t _) <- windows]) \t -> do
        reduceDamage t 1
        withHost a \host -> placeDamage a host 1
      pure s
    _ -> VigilantGuard <$> runMessage msg a
