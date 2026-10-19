module LordOfTheRings.Ally.Cards.BrokIronfist (brokIronfist) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf))

newtype BrokIronfist = BrokIronfist AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Response: After a Dwarf hero you control leaves play, put Brok Ironfist into play
-- from your hand.
brokIronfist :: AllyCard BrokIronfist
brokIronfist =
  ally (\a -> BrokIronfist a {allyHandWindow = Just $ HeroLeftPlayWindow After a.owner Dwarf}) Cards.brokIronfist
