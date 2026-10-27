module LordOfTheRings.Attachment.Cards.BootsFromErebor (bootsFromErebor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf, Hobbit))

newtype BootsFromErebor = BootsFromErebor AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf or Hobbit character. Limit 1 per character, as its card def says.
bootsFromErebor :: AttachmentCard BootsFromErebor
bootsFromErebor =
  attachment BootsFromErebor Cards.bootsFromErebor
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dwarf, HeroWithTrait Hobbit])
          (AllyMatchAny [AllyWithTrait Dwarf, AllyWithTrait Hobbit])
      )

-- | Attached character gets +1 hit point.
instance HasModifiersFor BootsFromErebor where
  getModifiersFor (BootsFromErebor a) = withHost a \host -> modified a host [HitPointModifier 1]
