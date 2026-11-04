module LordOfTheRings.Event.Cards.WillOfTheWest (willOfTheWest) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (eventsMayShuffleDiscardIntoDeck)

newtype WillOfTheWest = WillOfTheWest EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. Shuffle that player's discard pile back into his deck. Remove
-- Will of the West from the game. (FAQ 1.9 errata; as printed, it stays in the discard pile
-- and is not shuffled in, as it goes there only once it has resolved.) No discard pile is
-- shuffled while event cards cannot shuffle one, as Passage into Mordor reads.
willOfTheWest :: EventCard WillOfTheWest
willOfTheWest = event WillOfTheWest Cards.willOfTheWest

instance RunMessage WillOfTheWest where
  runMessage msg e@(WillOfTheWest attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      -- out of the game before the discard pile it sits in is shuffled
      errata <- playsWithErrata pid
      removeFromDiscard pid card.id
      whenM eventsMayShuffleDiscardIntoDeck $ choosePlayerM pid attrs \p -> push $ ShuffleDiscardIntoDeck p
      unless errata $ push $ PutInDiscard card
      pure e
    _ -> WillOfTheWest <$> runMessage msg attrs
