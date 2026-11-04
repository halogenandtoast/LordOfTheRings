module LordOfTheRings.Ally.Cards.Skinbark (skinbark) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype Skinbark = Skinbark AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
skinbark :: AllyCard Skinbark
skinbark = ally (Skinbark . (exhaustedL .~ True)) Cards.skinbark

-- | Cannot have restricted attachments. While Skinbark is attacking alone against an Orc
-- enemy, that enemy does not count its defense.
instance HasModifiersFor Skinbark where
  getModifiersFor (Skinbark a) =
    modifySelf a [CannotHaveRestrictedAttachments, IgnoresDefenseOfWhenAttackingAlone (EnemyWithTrait Orc)]
