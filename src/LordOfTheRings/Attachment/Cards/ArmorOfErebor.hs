module LordOfTheRings.Attachment.Cards.ArmorOfErebor (armorOfErebor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Trait (Trait (Dale, Dwarf))

newtype ArmorOfErebor = ArmorOfErebor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dale or Dwarf character. Restricted, on its card def.
armorOfErebor :: AttachmentCard ArmorOfErebor
armorOfErebor =
  attachment ArmorOfErebor Cards.armorOfErebor
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dale, HeroWithTrait Dwarf])
          (AllyMatchAny [AllyWithTrait Dale, AllyWithTrait Dwarf])
      )

-- | Attached character gets +1 defense and gains sentinel.
instance HasModifiersFor ArmorOfErebor where
  getModifiersFor (ArmorOfErebor a) = withHost a \host -> modified a host [DefenseModifier 1, GainKeyword Sentinel]
