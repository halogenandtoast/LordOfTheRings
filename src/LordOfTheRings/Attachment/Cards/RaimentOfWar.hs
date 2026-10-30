module LordOfTheRings.Attachment.Cards.RaimentOfWar (raimentOfWar) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Warrior))

newtype RaimentOfWar = RaimentOfWar AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Warrior character. Raiment of War counts as 2 Restricted attachments, on
-- its card def.
raimentOfWar :: AttachmentCard RaimentOfWar
raimentOfWar =
  attachment RaimentOfWar Cards.raimentOfWar
    & attachTo (AttachToCharacterMatching (HeroWithTrait Warrior) (AllyWithTrait Warrior))

-- | Attached character gets +1 attack, +1 defense and +2 hit points.
instance HasModifiersFor RaimentOfWar where
  getModifiersFor (RaimentOfWar a) = withHost a \host ->
    modified a host [AttackModifier 1, DefenseModifier 1, HitPointModifier 2]
