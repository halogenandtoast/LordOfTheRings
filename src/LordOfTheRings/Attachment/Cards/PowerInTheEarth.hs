module LordOfTheRings.Attachment.Cards.PowerInTheEarth (powerInTheEarth) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype PowerInTheEarth = PowerInTheEarth AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

powerInTheEarth :: AttachmentCard PowerInTheEarth
powerInTheEarth = attachment PowerInTheEarth Cards.powerInTheEarth & attachTo (AttachToLocation AnyLocation)

-- | Attached location gets -1 threat.
instance HasModifiersFor PowerInTheEarth where
  getModifiersFor (PowerInTheEarth a) = case a.placement of
    AttachedTo host -> modified a host [ThreatModifier (-1)]
    _ -> pure ()
