module LordOfTheRings.Attachment.Cards.BowOfTheGaladhrim (bowOfTheGaladhrim) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Trait (Trait (Silvan))

newtype BowOfTheGaladhrim = BowOfTheGaladhrim AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted. Attach to a Silvan character with the ranged keyword.
bowOfTheGaladhrim :: AttachmentCard BowOfTheGaladhrim
bowOfTheGaladhrim =
  attachment BowOfTheGaladhrim Cards.bowOfTheGaladhrim
    & attachTo (AttachToCharacterMatching (HeroWithTrait Silvan <> HeroWithKeyword Ranged) (AllyWithTrait Silvan <> AllyWithKeyword Ranged))

-- | Attached character gets +1 attack (+2 attack instead if attacking an enemy not
-- engaged with you.)
instance HasModifiersFor BowOfTheGaladhrim where
  getModifiersFor (BowOfTheGaladhrim a) = withHost a \host ->
    modified a host [AttackModifier 1, AttackBonusAgainstMatching (not_ $ EnemyEngagedWith $ PlayerWithId a.owner) 1]
