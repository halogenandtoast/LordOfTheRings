-- | Cards the game makes that are not printed cards of their own.
module LordOfTheRings.Card.Synthetic (orcGuard, syntheticCards) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Prelude

-- | A player card placed face down to fight as an enemy (Escape from Dol Guldur): 1 hit
-- point, 1 attack and 1 defense. None are shuffled into a deck.
orcGuard :: CardDef
orcGuard = enemy "OrcGuard" "Orc Guard" EscapeFromDolGuldur 0 0 0 1 1 1

syntheticCards :: Map CardCode CardDef
syntheticCards = Map.fromList [(cdCardCode orcGuard, orcGuard)]
