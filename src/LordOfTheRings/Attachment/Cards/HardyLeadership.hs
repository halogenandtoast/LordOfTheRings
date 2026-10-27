module LordOfTheRings.Attachment.Cards.HardyLeadership (hardyLeadership) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype HardyLeadership = HardyLeadership AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a leadership hero.
hardyLeadership :: AttachmentCard HardyLeadership
hardyLeadership = attachment HardyLeadership Cards.hardyLeadership & attachTo (AttachToHero $ HeroWithSphere Leadership)

-- | Each Dwarf gets +1 hit point.
instance HasModifiersFor HardyLeadership where
  getModifiersFor (HardyLeadership a) = withHost a \_ -> do
    dwarves <- selectCharacters (HeroWithTrait Dwarf) (AllyWithTrait Dwarf)
    for_ dwarves \c -> modified a c [HitPointModifier 1]
