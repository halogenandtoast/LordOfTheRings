-- | What the cards of The Dunland Trap share: the Dunlendings each player is set upon by.
module LordOfTheRings.Scenario.Scenarios.TheDunlandTrap.Helpers (
  enemyWithTrait,
  eachPlayerSearchesToEngage,
) where

import LordOfTheRings.Card.CardType (CardType (EnemyType))
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait)

-- | An enemy card with the trait, as "a Boar Clan enemy" reads.
enemyWithTrait :: Trait -> CardMatcher
enemyWithTrait trait = CardWithType EnemyType <> CardWithTrait trait

{- | Each player searches the encounter deck and discard pile for an enemy with the trait
and puts it into play engaged with him; the deck is shuffled after each search.
-}
eachPlayerSearchesToEngage :: (ReverseQueue m, Targetable source) => source -> Trait -> m ()
eachPlayerSearchesToEngage source trait =
  eachPlayer \p -> searchEncounterDeckAndDiscardToEngage p source (enemyWithTrait trait)
