module LordOfTheRings.Attachment.Cards.EntanglingNets (entanglingNets) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype EntanglingNets = EntanglingNets AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play Entangling Nets into the staging area unattached. If unattached, attach
-- Entangling Nets to the next eligible enemy that enters the staging area.
entanglingNets :: AttachmentCard EntanglingNets
entanglingNets = attachment EntanglingNets Cards.entanglingNets & attachTo PlayIntoStagingArea

-- | Attached enemy gets -2 attack and -2 defense.
instance HasModifiersFor EntanglingNets where
  getModifiersFor (EntanglingNets a) = case a.placement of
    AttachedTo host -> modified a host [AttackModifier (-2), DefenseModifier (-2)]
    _ -> pure ()
