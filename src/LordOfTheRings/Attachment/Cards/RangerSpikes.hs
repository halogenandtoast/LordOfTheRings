module LordOfTheRings.Attachment.Cards.RangerSpikes (rangerSpikes) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype RangerSpikes = RangerSpikes AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play Ranger Spikes into the staging area unattached. If unattached, attach Ranger
-- Spikes to the next eligible enemy that enters the staging area.
rangerSpikes :: AttachmentCard RangerSpikes
rangerSpikes = attachment RangerSpikes Cards.rangerSpikes & attachTo PlayIntoStagingArea

-- | Players do not make engagement checks against attached enemy. Attached enemy gets -2
-- threat.
instance HasModifiersFor RangerSpikes where
  getModifiersFor (RangerSpikes a) = case a.placement of
    AttachedTo host -> modified a host [NoEngagementChecks, ThreatModifier (-2)]
    _ -> pure ()
