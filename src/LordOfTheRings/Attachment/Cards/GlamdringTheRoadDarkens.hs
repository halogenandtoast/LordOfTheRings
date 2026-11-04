module LordOfTheRings.Attachment.Cards.GlamdringTheRoadDarkens (glamdringTheRoadDarkens) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc, Warrior))

newtype GlamdringTheRoadDarkens = GlamdringTheRoadDarkens AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A boon of The Road Darkens. Attach to a Warrior hero or Gandalf. Restricted, on its
-- card def.
glamdringTheRoadDarkens :: AttachmentCard GlamdringTheRoadDarkens
glamdringTheRoadDarkens =
  attachment GlamdringTheRoadDarkens Cards.glamdringTheRoadDarkens
    & attachTo (AttachToCharacterMatching (HeroMatchAny [HeroWithTrait Warrior, HeroWithTitle "Gandalf"]) (AllyWithTitle "Gandalf"))

-- | Attached character gets +2 [attack].
instance HasModifiersFor GlamdringTheRoadDarkens where
  getModifiersFor (GlamdringTheRoadDarkens a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached character destroys an Orc enemy, draw 1 card.
instance HasAbilities GlamdringTheRoadDarkens where
  getAbilities (GlamdringTheRoadDarkens a) =
    [mkAbility a 1 $ response $ EnemyWithTraitDestroyedByAttackWindow After Orc host | AttachedTo host <- [a.placement]]

instance RunMessage GlamdringTheRoadDarkens where
  runMessage msg s@(GlamdringTheRoadDarkens a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> GlamdringTheRoadDarkens <$> runMessage msg a
