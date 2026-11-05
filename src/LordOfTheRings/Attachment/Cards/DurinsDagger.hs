module LordOfTheRings.Attachment.Cards.DurinsDagger (durinsDagger) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype DurinsDagger = DurinsDagger AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | An Ered Mithrin treasure. Attach to a character. You may play Durin's Dagger from your
-- discard pile (its card def's 'FromHandOrDiscard').
durinsDagger :: AttachmentCard DurinsDagger
durinsDagger = attachment DurinsDagger Cards.durinsDagger & attachTo AttachToCharacter

-- | Attached character gets +1 [attack].
instance HasModifiersFor DurinsDagger where
  getModifiersFor (DurinsDagger a) = withHost a \host -> modified a host [AttackModifier 1]
