module LordOfTheRings.Ally.Cards.DunedainWanderer (dunedainWanderer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype DunedainWanderer = DunedainWanderer AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged. Sentinel. Secrecy 3. All keywords, on its card def.
dunedainWanderer :: AllyCard DunedainWanderer
dunedainWanderer = ally DunedainWanderer Cards.dunedainWanderer
