module LordOfTheRings.Attachment.Cards.ThrorsHuntingBow (throrsHuntingBow) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype ThrorsHuntingBow = ThrorsHuntingBow AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The treasure Thrór's Hunting Bow of The Lonely Mountain. Attach to a hero. Restricted,
-- on its card def.
throrsHuntingBow :: AttachmentCard ThrorsHuntingBow
throrsHuntingBow = attachment ThrorsHuntingBow Cards.throrsHuntingBow

-- | Attached hero gains Ranged. When making a ranged attack, attached hero gets +2
-- [attack]: one on an enemy not engaged with its controller, as Rivendell Bow reads it.
instance HasModifiersFor ThrorsHuntingBow where
  getModifiersFor (ThrorsHuntingBow a) = withHost a \host ->
    modified a host [GainKeyword Ranged, AttackBonusAgainstMatching (not_ $ EnemyEngagedWith $ PlayerWithId a.controller) 2]
