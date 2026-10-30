-- | What the cards of Deadmen's Dike and its encounter sets read off a player's deck and
-- discard pile.
module LordOfTheRings.Scenario.Scenarios.DeadmensDike.Helpers (
  printedCost,
  cardTitle,
  copyInOwnersDiscard,
  discardTopCards,
  thaurdirMatcher,
  encounterDiscardPile,
) where

import LordOfTheRings.Card (Card, CardCost (StaticCost), CardDef, EncounterCard, cdCost, cdName, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (Entity (toAttrs))
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Target

-- | The card's printed cost: 0 for a hero, or a cost of X.
printedCost :: Card -> Int
printedCost c = case cdCost (toCardDef c) of
  Just (StaticCost n) -> n
  _ -> 0

-- | The card's title, its subtitle aside: cards of one title are copies of each other.
cardTitle :: CardDef -> Text
cardTitle def = let Name t _ = cdName def in t

-- | Whether a copy of the character, a card of its title, is in its owner's discard pile.
copyInOwnersDiscard :: HasGame m => Target -> m Bool
copyInOwnersDiscard = \case
  HeroTarget hid -> getHero hid >>= \h -> inDiscard h.owner (toCardDef h)
  AllyTarget aid -> getAlly aid >>= \a -> inDiscard a.owner (toCardDef a)
  _ -> pure False
 where
  inDiscard pid def = any ((== cardTitle def) . cardTitle . toCardDef) . (.discard) <$> getPlayer pid

-- | The player discards the top N cards of his deck, which are returned for the card to read.
discardTopCards :: ReverseQueue m => PlayerId -> Int -> m [Card]
discardTopCards pid n = do
  top <- take n . (.deck) <$> getPlayer pid
  discardTopOfDeck pid n
  pure top

thaurdirMatcher :: EnemyMatcher
thaurdirMatcher = EnemyWithCardCode (toCardCode Enemies.thaurdirDeadmensDike)

-- | The encounter discard pile, its topmost card first.
encounterDiscardPile :: HasGame m => m [EncounterCard]
encounterDiscardPile = getsGame ((.encounterDiscard) . toAttrs . (.scenario))
