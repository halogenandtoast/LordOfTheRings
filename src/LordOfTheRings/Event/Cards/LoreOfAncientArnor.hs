module LordOfTheRings.Event.Cards.LoreOfAncientArnor (loreOfAncientArnor) where

import LordOfTheRings.Card (Card)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (eventsMayShuffleDiscardIntoDeck)

newtype LoreOfAncientArnor = LoreOfAncientArnor EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | A boon of the Angmar Awakened campaign, from The Battle of Carn Dûm.
loreOfAncientArnor :: EventCard LoreOfAncientArnor
loreOfAncientArnor = event LoreOfAncientArnor Cards.loreOfAncientArnor

-- | Action: Each player returns 1 card from their discard pile to their hand (2 cards
-- instead if you are the only player in the game) and shuffles the rest of their discard
-- pile back into their deck. Then, remove this card from the game. The rest stays while event
-- cards cannot shuffle a discard pile, as Passage into Mordor reads.
instance RunMessage LoreOfAncientArnor where
  runMessage msg e@(LoreOfAncientArnor attrs) = case msg of
    PlayEvent _ card _ | card.id == toId attrs -> do
      players <- getRemainingPlayers
      shuffles <- eventsMayShuffleDiscardIntoDeck
      let n = if length players == 1 then 2 else 1
      for_ players \p -> do
        discarded <- (.discard) <$> getPlayer p
        returnCards p n [c | c <- discarded, c.id /= card.id]
        when shuffles $ shuffleDiscardIntoDeck p
      removeSourceCardFromGame (toSource attrs)
      pure e
    _ -> LoreOfAncientArnor <$> runMessage msg attrs

-- | The player returns so many different cards from their discard pile to their hand, one
-- at a time.
returnCards :: ReverseQueue m => PlayerId -> Int -> [Card] -> m ()
returnCards p n cards = when (n > 0) $ chooseOrRunOneM p do
  questionLabeled "loreOfAncientArnor.return"
  for_ cards \c -> targeting (CardIdTarget c.id) do
    returnFromDiscardToHand p c.id
    returnCards p (n - 1) (filter ((/= c.id) . (.id)) cards)
