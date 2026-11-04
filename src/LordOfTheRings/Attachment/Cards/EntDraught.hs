module LordOfTheRings.Attachment.Cards.EntDraught (entDraught) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Ent))

newtype EntDraught = EntDraught AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Play only if you control at least 1 Ent character. Attach to a character. Limit 1 per
-- character, on its card def.
entDraught :: AttachmentCard EntDraught
entDraught =
  attachment EntDraught Cards.entDraught
    & attachTo AttachToCharacter
    & playCriteria \pid ->
      let you = PlayerWithId pid
       in CharactersAtLeast 1 (HeroControlledBy you <> HeroWithTrait Ent) (AllyControlledBy you <> AllyWithTrait Ent)

-- | Attached character gets +2 hit points.
instance HasModifiersFor EntDraught where
  getModifiersFor (EntDraught a) = withHost a \host -> modified a host [HitPointModifier 2]
