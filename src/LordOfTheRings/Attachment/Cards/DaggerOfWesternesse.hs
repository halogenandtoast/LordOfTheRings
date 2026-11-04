module LordOfTheRings.Attachment.Cards.DaggerOfWesternesse (daggerOfWesternesse) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DaggerOfWesternesse = DaggerOfWesternesse AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted, on its card def. Attach to a hero.
daggerOfWesternesse :: AttachmentCard DaggerOfWesternesse
daggerOfWesternesse = attachment DaggerOfWesternesse Cards.daggerOfWesternesse & attachTo (AttachToHero AnyHero)

-- | Attached hero gets +1 [attack] (+2 [attack] instead if attacking an enemy with an
-- engagement cost higher than your threat).
instance HasModifiersFor DaggerOfWesternesse where
  getModifiersFor (DaggerOfWesternesse a) = withHost a \host ->
    modified a host [AttackModifier 1, AttackBonusAgainstMatching (EnemyWithEngagementCostAboveThreatOf a.controller) 1]
