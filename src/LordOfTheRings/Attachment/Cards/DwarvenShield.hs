module LordOfTheRings.Attachment.Cards.DwarvenShield (dwarvenShield) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype DwarvenShield = DwarvenShield AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf hero. Restricted, on its card def.
dwarvenShield :: AttachmentCard DwarvenShield
dwarvenShield = attachment DwarvenShield Cards.dwarvenShield & attachTo (AttachToHero $ HeroWithTrait Dwarf)

-- | Attached hero gets +1 defense.
instance HasModifiersFor DwarvenShield where
  getModifiersFor (DwarvenShield a) = case a.placement of
    AttachedTo host -> modified a host [DefenseModifier 1]
    _ -> pure ()

-- | Response: After attached hero takes damage from an enemy attack, exhaust Dwarven Shield
-- to add 1 resource to attached hero's pool.
instance HasAbilities DwarvenShield where
  getAbilities (DwarvenShield a) =
    [mkAbility a 1 $ ResponseAbility (DamagedByEnemyAttackWindow After host) (exhaustCost a) | AttachedTo host <- [a.placement]]

instance RunMessage DwarvenShield where
  runMessage msg s@(DwarvenShield a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \h -> placeResources a h 1
      pure s
    _ -> DwarvenShield <$> runMessage msg a
