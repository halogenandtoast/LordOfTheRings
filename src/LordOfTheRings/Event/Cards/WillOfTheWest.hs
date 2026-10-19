module LordOfTheRings.Event.Cards.WillOfTheWest (willOfTheWest) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype WillOfTheWest = WillOfTheWest EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose a player. Shuffle that player's discard pile back into his deck. Remove
-- Will of the West from the game.
willOfTheWest :: EventCard WillOfTheWest
willOfTheWest = event WillOfTheWest Cards.willOfTheWest

instance RunMessage WillOfTheWest where
  runMessage msg e@(WillOfTheWest attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      -- out of the game before the discard pile it sits in is shuffled
      removeFromDiscard pid card.id
      choosePlayerM pid attrs \p -> push $ ShuffleDiscardIntoDeck p
      pure e
    _ -> WillOfTheWest <$> runMessage msg attrs
