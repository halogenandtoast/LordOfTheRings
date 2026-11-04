module LordOfTheRings.Attachment.Cards.GlamdringTreasure (glamdringTreasure) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype GlamdringTreasure = GlamdringTreasure AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Glamdring of We Must Away, Ere Break of Day. Attach to a hero or Gandalf.
-- Restricted, on its card def.
glamdringTreasure :: AttachmentCard GlamdringTreasure
glamdringTreasure =
  attachment GlamdringTreasure Cards.glamdringTreasure & attachTo (AttachToCharacterMatching AnyHero (AllyWithTitle "Gandalf"))

-- | Attached character gets +2 [attack].
instance HasModifiersFor GlamdringTreasure where
  getModifiersFor (GlamdringTreasure a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached character destroys an Orc enemy, draw 1 card.
instance HasAbilities GlamdringTreasure where
  getAbilities (GlamdringTreasure a) =
    [mkAbility a 1 $ response $ EnemyWithTraitDestroyedByAttackWindow After Orc host | AttachedTo host <- [a.placement]]

instance RunMessage GlamdringTreasure where
  runMessage msg s@(GlamdringTreasure a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> GlamdringTreasure <$> runMessage msg a
