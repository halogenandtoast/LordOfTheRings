module LordOfTheRings.Attachment.Cards.SpearOfTheMark (spearOfTheMark) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype SpearOfTheMark = SpearOfTheMark AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

spearOfTheMark :: AttachmentCard SpearOfTheMark
spearOfTheMark =
  attachment SpearOfTheMark Cards.spearOfTheMark
    & attachTo (AttachToCharacterMatching (HeroWithTrait Rohan) (AllyWithTrait Rohan))

-- | Attached character gets +1 attack (+2 attack instead if attacking an enemy in the
-- staging area).
instance HasModifiersFor SpearOfTheMark where
  getModifiersFor (SpearOfTheMark a) = withHost a \host ->
    modified a host [AttackModifier 1, AttackBonusAgainstMatching EnemyInStagingArea 1]
