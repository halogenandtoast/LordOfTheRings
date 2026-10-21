module LordOfTheRings.Attachment.Cards.IthilienPit (ithilienPit) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype IthilienPit = IthilienPit AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play Ithilien Pit into the staging area unattached. If unattached, attach Ithilien
-- Pit to the next eligible enemy that enters the staging area.
ithilienPit :: AttachmentCard IthilienPit
ithilienPit = attachment IthilienPit Cards.ithilienPit & attachTo PlayIntoStagingArea

-- | Any character may choose attached enemy as the target of an attack.
instance HasModifiersFor IthilienPit where
  getModifiersFor (IthilienPit a) = case a.placement of
    AttachedTo host -> modified a host [AttackableByAnyCharacter]
    _ -> pure ()
