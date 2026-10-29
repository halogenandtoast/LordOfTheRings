module LordOfTheRings.Attachment.Cards.CloakOfLorien (cloakOfLorien) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Forest, Noldor, Silvan))

newtype CloakOfLorien = CloakOfLorien AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor or Silvan character. Limit 1 per character, on its card def.
cloakOfLorien :: AttachmentCard CloakOfLorien
cloakOfLorien =
  attachment CloakOfLorien Cards.cloakOfLorien
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan])
          (AllyMatchAny [AllyWithTrait Noldor, AllyWithTrait Silvan])
      )

-- | Attached character gets +1 defense (+2 defense instead if the active location has the
-- Forest trait).
instance HasModifiersFor CloakOfLorien where
  getModifiersFor (CloakOfLorien a) = withHost a \host -> do
    forest <- selectAny (IsActiveLocation <> LocationWithTrait Forest)
    modified a host [DefenseModifier $ if forest then 2 else 1]
