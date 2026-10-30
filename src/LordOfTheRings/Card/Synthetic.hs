-- | Cards the game makes that are not printed cards of their own.
module LordOfTheRings.Card.Synthetic (orcGuard, reanimatedDead, syntheticCards) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Prelude
import LordOfTheRings.Trait (Trait (Undead))

-- | A player card placed face down to fight as an enemy (Escape from Dol Guldur): 1 hit
-- point, 1 attack and 1 defense. None are shuffled into a deck.
orcGuard :: CardDef
orcGuard = enemy "OrcGuard" "Orc Guard" EscapeFromDolGuldur 0 0 0 1 1 1

-- | A player card reanimated face down in the staging area (The Dread Realm): an Undead
-- enemy with 2 threat, 2 attack, 2 defense and 2 hit points, as each quest card reads.
-- The quest cards give it no engagement cost, which the engine counts as 0.
reanimatedDead :: CardDef
reanimatedDead = (enemy "ReanimatedDead" "Reanimated Dead" TheDreadRealm 0 0 2 2 2 2) {cdCardTraits = setFromList [Undead], cdEngagementCost = Nothing}

syntheticCards :: Map CardCode CardDef
syntheticCards = Map.fromList [(cdCardCode c, c) | c <- [orcGuard, reanimatedDead]]
