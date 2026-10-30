module LordOfTheRings.Attachment.Cards.RangerSpear (rangerSpear) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger))

newtype RangerSpear = RangerSpear AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Ranger character. Restricted, on its card def.
rangerSpear :: AttachmentCard RangerSpear
rangerSpear =
  attachment RangerSpear Cards.rangerSpear
    & attachTo (AttachToCharacterMatching (HeroWithTrait Ranger) (AllyWithTrait Ranger))

-- | Attached character gets +1 attack (+2 attack instead when attacking an enemy with an
-- attachment on it).
instance HasModifiersFor RangerSpear where
  getModifiersFor (RangerSpear a) = withHost a \host ->
    modified a host [AttackModifier 1, AttackBonusAgainstMatching EnemyWithAnyAttachment 1]
