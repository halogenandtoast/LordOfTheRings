module LordOfTheRings.Event.Cards.DunedainMessage (dunedainMessage) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype DunedainMessage = DunedainMessage EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Search your deck for a side quest and add it to your hand. Shuffle your deck.
dunedainMessage :: EventCard DunedainMessage
dunedainMessage = event DunedainMessage Cards.dunedainMessage

instance RunMessage DunedainMessage where
  runMessage msg e@(DunedainMessage attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      deckSize <- length . (.deck) <$> getPlayer pid
      searchDeck pid attrs deckSize (CardWithType PlayerSideQuestType) False
      pure e
    _ -> DunedainMessage <$> runMessage msg attrs
