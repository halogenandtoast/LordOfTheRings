module LordOfTheRings.Attachment.Cards.RivendellBow (rivendellBow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (cdKeywords)
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype RivendellBow = RivendellBow AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor or Silvan character, or to Aragorn. Limit 1 per character, as its
-- card def says.
rivendellBow :: AttachmentCard RivendellBow
rivendellBow =
  attachment RivendellBow Cards.rivendellBow
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan, HeroWithTitle "Aragorn"])
          (AllyMatchAny [AllyWithTrait Noldor, AllyWithTrait Silvan, AllyWithTitle "Aragorn"])
      )

-- | Attached character gains ranged. If attached character has the printed ranged
-- keyword, it gets +1 attack during a ranged attack: one on an enemy not engaged with
-- its controller, as Bow of the Galadhrim reads it.
instance HasModifiersFor RivendellBow where
  getModifiersFor (RivendellBow a) = withHost a \host -> do
    printed <- member Ranged . cdKeywords <$> characterCardDef host
    modified a host
      $ GainKeyword Ranged
      : [AttackBonusAgainstMatching (not_ $ EnemyEngagedWith $ PlayerWithId a.controller) 1 | printed]
