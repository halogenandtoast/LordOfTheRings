module LordOfTheRings.Attachment.Cards.HauberkOfMail (hauberkOfMail) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Helpers.Character (characterCardDef)
import LordOfTheRings.Keyword (Keyword (Sentinel))
import LordOfTheRings.Trait (Trait (Dale, Warrior))

newtype HauberkOfMail = HauberkOfMail AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dale or Warrior character. Limit 1 per character.
hauberkOfMail :: AttachmentCard HauberkOfMail
hauberkOfMail =
  attachment HauberkOfMail Cards.hauberkOfMail
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Dale, HeroWithTrait Warrior])
          (AllyMatchAny [AllyWithTrait Dale, AllyWithTrait Warrior])
      )

-- | Attached character gets +1 defense (and +1 hit point if attached character has the
-- sentinel keyword).
instance HasModifiersFor HauberkOfMail where
  getModifiersFor (HauberkOfMail a) = case a.placement of
    AttachedTo host -> do
      sentinel <- hasKeyword Sentinel host =<< characterCardDef host
      modified a host $ DefenseModifier 1 : [HitPointModifier 1 | sentinel]
    _ -> pure ()
