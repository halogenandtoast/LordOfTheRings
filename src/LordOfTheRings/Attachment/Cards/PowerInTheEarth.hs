module LordOfTheRings.Attachment.Cards.PowerInTheEarth (powerInTheEarth) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype PowerInTheEarth = PowerInTheEarth AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

powerInTheEarth :: AttachmentCard PowerInTheEarth
powerInTheEarth = attachment PowerInTheEarth Cards.powerInTheEarth & attachTo (AttachToLocation AnyLocation)
