module LordOfTheRings.Ally.Cards.WanderingEnt (wanderingEnt) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype WanderingEnt = WanderingEnt AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Enters play exhausted.
wanderingEnt :: AllyCard WanderingEnt
wanderingEnt = ally (WanderingEnt . (exhaustedL .~ True)) Cards.wanderingEnt

-- | Cannot have restricted attachments.
instance HasModifiersFor WanderingEnt where
  getModifiersFor (WanderingEnt a) = modifySelf a [CannotHaveRestrictedAttachments]
