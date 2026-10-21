module LordOfTheRings.Ally.Cards.HaldirOfLorien (haldirOfLorien) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype HaldirOfLorien = HaldirOfLorien AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged. Sentinel.
haldirOfLorien :: AllyCard HaldirOfLorien
haldirOfLorien = ally HaldirOfLorien Cards.haldirOfLorien
