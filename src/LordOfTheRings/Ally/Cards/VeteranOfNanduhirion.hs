module LordOfTheRings.Ally.Cards.VeteranOfNanduhirion (veteranOfNanduhirion) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype VeteranOfNanduhirion = VeteranOfNanduhirion AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Veteran of Nanduhirion enters play with 1 damage on it: it is built with it, so no
-- damage is dealt.
veteranOfNanduhirion :: AllyCard VeteranOfNanduhirion
veteranOfNanduhirion =
  ally (\a -> VeteranOfNanduhirion a {allyTokens = addTokens Damage 1 a.allyTokens}) Cards.veteranOfNanduhirion
